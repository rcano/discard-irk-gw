package discordircgw

import fastparse.all._

object IrcParser {
  val noSpace = CharsWhile(!_.isSpaceChar)
  val message = P((":" ~ prefix.! ~ " ".rep).?.log() ~ command.!.log() ~ params.log())
  val prefix = P(noSpace.! /*servername or nick*/ ~ ("!" ~ noSpace.! /*user*/ ).? ~ ("@" ~ noSpace.! /*host*/ ).?)
  val command = P(CharsWhile(_.isLetter) | CharIn('0' to '9').rep(exactly = 3))
  val params = P(" ".rep ~ middle.!.rep(sep = " ".rep).log() ~ (" ".rep ~ ":".log() ~ trailing.!.log()).?)
  val middle = !":" ~ CharsWhile(c => c != ' ' && c != 0 && c != '\n' && c != '\r')
  val trailing = CharsWhile(c => c != 0 && c != '\n' && c != '\r', 0)

  case class IrcEvent(prefix: Option[String], cmd: String, args: Seq[String])
  def apply(s: String) = message.parse(s) match {
    case f: Parsed.Failure => Left(ParseError(f).getMessage)
    case Parsed.Success((prefix, cmd, (args, last)), _) => Right(IrcEvent(prefix, cmd, args ++ last))
  }
}
