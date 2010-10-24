package unfiltered.netty.channel

import org.jboss.netty.handler.codec.http.{DefaultHttpRequest,DefaultHttpResponse}
import org.jboss.netty.channel._
import unfiltered.netty._
import unfiltered.response.NotFound
import unfiltered.request.HttpRequest

object Plan {
  type Intent = PartialFunction[RecievedMessageBinding, Unit]
}
/** A Netty Plan for request only handling. */
abstract class Plan extends SimpleChannelUpstreamHandler {
  def intent: Plan.Intent
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request = e.getMessage().asInstanceOf[DefaultHttpRequest]
    val messageBinding = new RecievedMessageBinding(request, ctx, e)
    if (intent.isDefinedAt(messageBinding)) {
      intent(messageBinding)
    } else {
      messageBinding.respond(NotFound)
    }
  }
}

class Planify(val intent: Plan.Intent) extends Plan

object Planify {
  def apply(intent: Plan.Intent) = new Planify(intent)
}
