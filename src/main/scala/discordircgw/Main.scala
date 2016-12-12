package discordircgw

import java.net.{ ServerSocket, Socket, InetAddress }
import net.dv8tion.jda.core.entities.TextChannel
import net.dv8tion.jda.core.entities.User
import net.dv8tion.jda.core.events.ReadyEvent
import net.dv8tion.jda.core.events.message.{ MessageReceivedEvent, MessageUpdateEvent }
import net.dv8tion.jda.core.hooks.EventListener
import net.dv8tion.jda.core.{ AccountType, JDABuilder }
import scala.collection.JavaConverters._

import IrcParser.IrcEvent

object Main extends App {

  case class CliArgs(token: String = null, ircPort: Int = 6667)

  val cliArgs = new scopt.OptionParser[CliArgs]("discord-irc-gw") {
    opt[String]('t', "token").required.text("user token (obtained snooping it from the web client, going to the console and doing localStorage.token).").
      action((x, c) => c.copy(token = x))
    opt[Int]('p', "ircPort").text("Port on which the IRC gw will be listening. defaults to 6667.").
      action((x, c) => c.copy(ircPort = x))
  }.parse(args, CliArgs()).getOrElse(sys.exit(1))

  val jda = new JDABuilder(AccountType.CLIENT).setAudioEnabled(false).setAutoReconnect(true).
    setToken(cliArgs.token).addListener(DiscordBot).buildAsync()

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
              try ircStateMachine.applyIfDefined(ircEvt)
              catch { case e: Exception => e.printStackTrace() }
          })
        }, "irc client").start()
        messageHandling(client)
    }

    def messageHandling(client: Socket): Transition = transition {
      case msg: MessageReceivedEvent => messageHandling(client)
      case msg: MessageUpdateEvent => messageHandling(client)

    }
  }
  class IrcStateMachine(clientOut: java.io.OutputStream) extends StateMachine[IrcEvent] {
    type MappedName = String
    case class MessageHandlingState(myUser: String, myNick: String,
      mappedUserNames: Map[String, Seq[(User, MappedName)]],
      mappedChannelNames: Map[String, TextChannel],
      joinedChannels: Seq[TextChannel])

    override def initState = connecting(None, None)
    def connecting(user: Option[String], nick: Option[String]): Transition = {
      if (user.isDefined && nick.isDefined) {
        sendSmsg(nick.get, 1, "Welcome")
        sendSmsg(nick.get, 376, "there was no MOTD.")
        messageHandling(MessageHandlingState(user.get, nick.get,
          jda.getUsers.asScala.groupBy(cleanNameForIrc).map(e => e._1 -> mapNames(e._2)),
          jda.getGuilds.asScala.flatMap(g => g.getTextChannels.asScala.map(c => (g.getName + "_" + c.getName).replace(' ', '_') -> c)).toMap,
          Seq.empty))
      } else transition {
        case IrcEvent(_, "USER", args) => connecting(Some(args.last), nick)
        case IrcEvent(_, "NICK", args) => connecting(user, Some(args.last))
      }
    }

    def messageHandling(state: MessageHandlingState): Transition = transition {
      case evt =>
        evt match {
          case IrcEvent(_, "JOIN", Seq(chans)) =>
            val joinedChannels = chans.split(",").map(_.stripPrefix("#")) flatMap { chan =>
              val found = state.mappedChannelNames.get(chan)
              found.fold(sendSmsg(state.myNick, 403, chan, "This channel does not exist in your server!")) { _ =>
                sendMsg(state.myNick, "JOIN", chan)
                messageHandling(state)(IrcEvent(null, "NAMES", Seq(chan)))
              }
              found
            }
            //delegate to names
            messageHandling(state.copy(joinedChannels = state.joinedChannels ++ joinedChannels))

          case IrcEvent(_, "NAMES", args) if args.isEmpty =>
            sendSmsg(state.myNick, 366, "*", "End of /NAMES list.")
            messageHandling(state)

          case IrcEvent(_, "NAMES", Seq(chan)) =>
            state.mappedChannelNames.get(chan) match {
              case Some(tc) =>
                val nicks = tc.getMembers.asScala.map(m => state.mappedUserNames.values.flatten.collectFirst {
                  case (user, mappedName) if user == m.getUser => mappedName
                }.getOrElse(m.getUser.getName))
                nicks.grouped(10) foreach (g => sendSmsg(state.myNick, 353, "=", chan, g.mkString(" ")))
                sendSmsg(state.myNick, 366, chan, "End of /NAMES list.")
              case None => sendSmsg(state.myNick, 401, chan, "You are not joined to that channel.")
            }
            messageHandling(state)

          case IrcEvent(_, "PRIVMSG", Seq(dest, msg)) =>
            if (dest.startsWith("#")) {
              state.joinedChannels
            } else {
              
            }
            
            messageHandling(state)
            
          case IrcEvent(_, "USERHOST", args) => 
            sendSmsg(state.myNick, 302, args.filter(_ == state.myNick).map(u => s"$u=${state.myUser}@127.0.0.1"):_*)
            messageHandling(state)

          case IrcEvent(_, "WHO", Seq(who)) =>
            if (who.startsWith("#")) {
              state.mappedChannelNames.get(who).foreach { channel =>
                channel.getMembers.asScala.foreach { m =>
                  val userName = m.getUser.getName
                  val mappedUserName = state.mappedUserNames.get(userName).flatMap(_.find(_._1.getName == userName).map(e => userName + e._2)).getOrElse(userName)
                  sendSmsg(state.myNick, 352, who, mappedUserName, "localhost", serverName, mappedUserName, "H", ":0", m.getNickname)
                }
              }
            } else {
              //get the name straight, if not found try removing the digits at the end because we might have mapped it
              state.mappedUserNames.get(who).orElse(state.mappedUserNames.get(who.reverse.dropWhile(_.isDigit).reverse)).foreach { users =>
                if (users.size == 1)
                  sendSmsg(state.myNick, 352, who, who, "localhost", serverName, who, "H", ":0", who)
                else users foreach { user =>
                  val userNick = user._2
                  sendSmsg(state.myNick, 352, who, userNick, "localhost", serverName, userNick, "H", ":0", userNick)
                }
              }
            }
            sendSmsg(state.myNick, 315, "End of /WHO list.")
            messageHandling(state)

          case IrcEvent(_, "PING", args) =>
            sendSmsg(state.myNick, "PONG", (serverName +: args.drop(1)): _*)
            messageHandling(state)

          case IrcEvent(_, other, args) =>
            sendSmsg(state.myNick, 302, "Not enough parameters.")
            messageHandling(state)
        }
    }

    private def cleanNameForIrc(u: User): String = u.getName.replaceAll("\\s", "_")
    private def mapNames(s: Seq[User]): Seq[(User, MappedName)] = {
      if (s.size == 1) s.map(u => u -> cleanNameForIrc(u))
      else s.zipWithIndex.map(e => e._1 -> (cleanNameForIrc(e._1) + e._2))
    }
    private def sendSmsg(nickname: String, cmd: Any, args: String*): Unit = {
      if (args.dropRight(1).exists(_.contains(' '))) throw new IllegalArgumentException("space in non-last command")
      val ircArgs = (args.dropRight(1) ++ args.lastOption.map(":" + _)).mkString(" ")
      send(s":$serverName $cmd $nickname $ircArgs")
    }
    private def sendMsg(userfrom: String, cmd: Any, args: String*): Unit = {
      if (args.dropRight(1).exists(_.contains(' '))) throw new IllegalArgumentException("space in non-last command")
      val ircArgs = (args.dropRight(1) ++ args.lastOption.map(":" + _)).mkString(" ")
      val resUserfrom = if (userfrom.charAt(0) != ':') s":$userfrom!$userfrom@localhost" else userfrom
      send(s"$resUserfrom $cmd $ircArgs")
    }
    private def send(s: String) = {
      println(s"sending $s")
      clientOut.write((s + "\r\n").getBytes("utf-8"))
    }
  }

}
