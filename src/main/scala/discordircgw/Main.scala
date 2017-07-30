package discordircgw

import java.net.{ ServerSocket, Socket, InetAddress }
import net.dv8tion.jda.core.entities.{ ChannelType, Message, MessageChannel, TextChannel, User }
import net.dv8tion.jda.core.events.ReadyEvent
import net.dv8tion.jda.core.events.message.{ MessageReceivedEvent, MessageUpdateEvent }
import net.dv8tion.jda.core.hooks.EventListener
import net.dv8tion.jda.core.{ AccountType, JDABuilder }
import scala.collection.JavaConverters._

object Main extends App {

  case class CliArgs(token: String = null, ircPort: Int = 6667)

  val cliArgs = new scopt.OptionParser[CliArgs]("discord-irc-gw") {
    opt[String]('t', "token").required.text("user token (obtained snooping it from the web client, going to the console and doing localStorage.token).").
      action((x, c) => c.copy(token = x))
    opt[Int]('p', "ircPort").text("Port on which the IRC gw will be listening. defaults to 6667.").
      action((x, c) => c.copy(ircPort = x))
  }.parse(args, CliArgs()).getOrElse(sys.exit(1))

  val jda = new JDABuilder(AccountType.CLIENT).setAudioEnabled(false).setAutoReconnect(true).
    setToken(cliArgs.token).addEventListener(DiscordBot).buildAsync()

  val serverName = "DiscordIrcGw"

  def println(a: Any) = Console.println(Console.BLUE + a + Console.RESET)

  object DiscordBot extends StateMachine[Any] with EventListener {
    override def apply(a: Any) = synchronized { super.apply(a) }
    def onEvent(event) = try {
      applyIfDefined(event)
    } catch { case e: Exception => e.printStackTrace() }

    override def initState = awaitReady

    def awaitReady = transition {
      case e: ReadyEvent =>
        val client = {
          println("waiting for IRC client")
          val ss = new ServerSocket(cliArgs.ircPort, 1, InetAddress.getLoopbackAddress)
          val res = ss.accept()
          println("IRC client found")
          ss.close()
          res
        }
        //plug the client's inputstream to the state machine in a dedicated thread
        val ircStateMachine = new IrcStateMachine(client.getOutputStream())
        new Thread(() => {
          import java.io._
          val in = new BufferedReader(new InputStreamReader(client.getInputStream))
          Iterator.continually(in.readLine()).takeWhile(_ != null).map { s => println(s); s }.foreach(s => IrcParser(s) match {
            case Left(err) => println("wut irc?: " + err)
            case Right(ircEvt) =>
              try ircStateMachine(ircEvt)
              catch { case e: Exception => e.printStackTrace() }
          })
        }, "irc client").start()
        messageHandling(client, ircStateMachine)
    }

    def messageHandling(client: Socket, clientStateMachine: IrcStateMachine): Transition = transition {
      case mre: MessageReceivedEvent if (mre.getAuthor) != jda.getSelfUser =>
        clientStateMachine(SendMessage(mre.getAuthor, mre.getChannel, mre.getMessage))
        messageHandling(client, clientStateMachine)
      case mue: MessageUpdateEvent if (mue.getAuthor) != jda.getSelfUser =>
        clientStateMachine(SendMessage(mue.getAuthor, mue.getChannel, mue.getMessage, true))
        messageHandling(client, clientStateMachine)

    }
  }
  class IrcStateMachine(clientOut: java.io.OutputStream) extends StateMachine[IrcClientEvent] {
    type MappedName = String
    case class MessageHandlingState(myUser: String, myNick: String,
      mappedUserNames: Map[MappedName, User],
      mappedChannelNames: Map[String, TextChannel],
      joinedChannels: Seq[TextChannel])

    override def initState = connecting(None, None)
    def connecting(user: Option[String], nick: Option[String]): Transition = {
      if (user.isDefined && nick.isDefined) {
        val state = MessageHandlingState(user.get, nick.get,
          jda.getUsers.asScala.groupBy(_.getName).flatMap(e => mapNames(e._2)).toMap,
          jda.getGuilds.asScala.flatMap(g => g.getTextChannels.asScala.map(c => ("#" + g.getName + "_" + c.getName).replace(' ', '_') -> c)).toMap,
          Seq.empty)

        val mappedChannelNames = state.mappedChannelNames.keys.toSeq.sorted
        sendSmsg(nick.get, 1, s"Welcome, here are your channels: ${mappedChannelNames.mkString(", ")}")
        sendSmsg(nick.get, 376, "there was no MOTD.")
        messageHandling(state)
      } else transition {
        case IrcEvent(_, "USER", args) => connecting(Some(args.last), nick)
        case IrcEvent(_, "NICK", args) => connecting(user, Some(args.last))
      }
    }

    def messageHandling(state: MessageHandlingState): Transition = transition {
      case evt =>
        evt match {
          case IrcEvent(_, "JOIN", Seq(chans)) =>
            val joinedChannels = chans.split(",") flatMap { chan =>
              val found = state.mappedChannelNames.get(chan)
              found.fold(sendSmsg(state.myNick, 403, chan, "This channel does not exist in your server!")) { _ =>
                sendMsg(state.myNick, "JOIN", chan)
                messageHandling(state)(IrcEvent(null, "NAMES", Seq(chan)))
              }
              found
            }
            //delegate to names
            val newJoinedChannels = state.joinedChannels ++ joinedChannels
            println("all joined channels: " + newJoinedChannels.map(_.getName).mkString(", "))
            messageHandling(state.copy(joinedChannels = newJoinedChannels))

          case IrcEvent(_, "NAMES", args) if args.isEmpty =>
            sendSmsg(state.myNick, 366, "*", "End of /NAMES list.")
            messageHandling(state)

          case IrcEvent(_, "NAMES", Seq(chan)) =>
            state.mappedChannelNames.get(chan) match {
              case Some(tc) =>
                val nicks = tc.getMembers.asScala.map(m => state.mappedUserNames.collectFirst {
                  case (mappedName, user) if user == m.getUser => mappedName
                }.getOrElse(cleanNameForIrc(m.getUser)))
                nicks.grouped(10) foreach (g => sendSmsg(state.myNick, 353, "=", chan, g.mkString(" ")))
                sendSmsg(state.myNick, 366, chan, "End of /NAMES list.")
              case None => sendSmsg(state.myNick, 401, chan, "You are not joined to that channel.")
            }
            messageHandling(state)

          case IrcEvent(_, "PRIVMSG", Seq(dest, msg)) if dest.startsWith("#") && !state.joinedChannels.contains(state.mappedChannelNames(dest)) =>
            sendSmsg(state.myNick, 401, "Destination channel not joined. Joined channels: " + state.joinedChannels.map(_.getName).mkString(", "))
            messageHandling(state)

          case IrcEvent(_, "PRIVMSG", Seq(dest, msg)) =>
            if (dest.startsWith("#")) {
              val chn = state.mappedChannelNames(dest)
              chn.sendMessage(msg).queue()
            } else {
              state.mappedUserNames.get(dest) match {
                case None => sendSmsg(state.myNick, 401, "User not found")
                case Some(user) =>
                  try {
                    user.openPrivateChannel().complete(true).sendMessage(msg).queue()
                  } catch {
                    case e: Exception => sendSmsg(state.myNick, 400, s"Could not open private channel with user: $e")
                  }
              }
            }

            messageHandling(state)

          case IrcEvent(_, "USERHOST", args) =>
            sendSmsg(state.myNick, 302, args.filter(_ == state.myNick).map(u => s"$u=${state.myUser}@127.0.0.1"): _*)
            messageHandling(state)

          case IrcEvent(_, "WHO", Seq(who)) =>
            if (who.startsWith("#")) {
              state.mappedChannelNames.get(who).foreach { channel =>
                channel.getMembers.asScala.foreach { m =>
                  val mappedUserName = state.mappedUserNames.collectFirst { case (mn, user) if user == m.getUser => mn }.getOrElse(cleanNameForIrc(m.getUser))
                  sendSmsg(state.myNick, 352, who, mappedUserName, "localhost", serverName, mappedUserName, "H", ":0", m.getNickname)
                }
              }
            } else {
              state.mappedUserNames.get(who) foreach { user =>
                sendSmsg(state.myNick, 352, who, who, "localhost", serverName, who, "H", ":0", who)
              }
            }
            sendSmsg(state.myNick, 315, "End of /WHO list.")
            messageHandling(state)

          case IrcEvent(_, "PING", args) =>
            sendSmsg(state.myNick, "PONG", (serverName +: args.drop(1)): _*)
            messageHandling(state)

          case IrcEvent(_, other, args) =>
            sendSmsg(state.myNick, 461, "Not enough parameters.")
            messageHandling(state)

          case SendMessage(author, channel, msg, update) if state.joinedChannels.contains(channel) || channel.getType == ChannelType.PRIVATE =>
            val mappedUser = state.mappedUserNames.find(_._2 == author).get._1
            val from = if (channel.getType == ChannelType.PRIVATE) mappedUser else state.mappedChannelNames.find(_._2 == channel).get._1
            val content = (if (update) "(edited) " else "") + msg.getContent
            sendMsg(mappedUser, "PRIVMSG", from, content)
            messageHandling(state)

          case _ => messageHandling(state)
        }
    }

    private def cleanNameForIrc(u: User): String = u.getName.replaceAll("\\s", "_")
    private def mapNames(s: Seq[User]): Seq[(MappedName, User)] = {
      if (s.size == 1) s.map(u => cleanNameForIrc(u) -> u)
      else s.zipWithIndex.map(e => (cleanNameForIrc(e._1) + e._2) -> e._1)
    }
    def sendSmsg(nickname: String, cmd: Any, args: String*): Unit = {
      if (args.dropRight(1).exists(_.contains(' '))) throw new IllegalArgumentException("space in non-last command")
      val ircArgs = (args.dropRight(1) ++ args.lastOption.map(":" + _)).mkString(" ")
      send(s":$serverName $cmd $nickname $ircArgs")
    }
    def sendMsg(userfrom: String, cmd: Any, args: String*): Unit = {
      if (args.dropRight(1).exists(_.contains(' '))) throw new IllegalArgumentException("space in non-last command")
      val ircArgs = (args.dropRight(1) ++ args.lastOption.map(":" + _)).mkString(" ")
      val resUserfrom = if (userfrom.charAt(0) != ':') s":$userfrom!$userfrom@localhost" else userfrom
      send(s"$resUserfrom $cmd $ircArgs")
    }
    def send(s: String) = {
      println(s"sending $s")
      clientOut.write((s + "\r\n").getBytes("utf-8"))
    }
  }

}

sealed trait IrcClientEvent
case class IrcEvent(prefix: Option[String], cmd: String, args: Seq[String]) extends IrcClientEvent
case class SendMessage(from: User, channel: MessageChannel, message: Message, updated: Boolean = false) extends IrcClientEvent