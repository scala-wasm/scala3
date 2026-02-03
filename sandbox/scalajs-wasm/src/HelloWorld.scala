package helloworld

import scala.scalajs.wit.annotation._
import scala.scalajs.wit

import helloworld.exports.wasi.cli.Run

import scala.scalajs.wasi.http.outgoing_handler
import scala.scalajs.wasi.http.types.{
  Fields,
  Method,
  OutgoingBody,
  OutgoingRequest,
  Scheme,
}

import java.util.Optional

@WitImplementation
object Main extends Run {
  import Test._

  @WitExport("wasi:cli/run@0.2.0", "run")
  def run(): wit.Result[Unit, Unit] = {

    val request = OutgoingRequest(headers = Fields())
    request.setMethod(Method.Post)
    request.setPathWithQuery(Optional.of("/posts"))
    request.setScheme(Optional.of(Scheme.Http))
    request.setAuthority(Optional.of("jsonplaceholder.typicode.com"))
    request.body() match {
      case ok: wit.Ok[?] =>
        val body = ok.value
        body.write() match {
          case ok2: wit.Ok[?] =>
            val os = ok2.value
            os.blockingWriteAndFlush(stringToBytes("""{"title":"foo","body":"bar","userId":1}"""))
            os.close()
            OutgoingBody.finish(body, Optional.empty())
          case _: wit.Err[?] =>
            println("body.write error, invariant violation")
        }
      case _: wit.Err[?] => println("request.body error, invariant violation")
    }

    val resFutIncRes = outgoing_handler.handle(
      request = request,
      options = Optional.empty()
    )

    resFutIncRes match {
      case ok: wit.Ok[?] =>
        val futIncRes = ok.value

        val pollable = futIncRes.subscribe()
        pollable.block()

        val resResIncRes = futIncRes.get().orElseThrow() // we polled so it should always be defined
        resResIncRes match {
          case ok2: wit.Ok[?] =>
            val resIncRes = ok2.value
            resIncRes match {
              case ok3: wit.Ok[?] =>
                val incRes = ok3.value
                println("Received response")
                println(s"Code = ${incRes.status()}")
                println("Headers:")
                incRes.headers().entries().foreach { tp2 =>
                  val k = tp2._1
                  val v = tp2._2
                  println(s"  $k -> ${bytesToString(v)}")
                }
                val resIncBody = incRes.consume()
                resIncBody match {
                  case ok4: wit.Ok[?] =>
                    val incBody = ok4.value
                    incBody.stream() match {
                      case ok5: wit.Ok[?] =>
                        val is = ok5.value

                        var body = ""

                        var running = true
                        while (running) {
                          is.blockingRead(1024) match {
                            case ok6: wit.Ok[?] =>
                              body = body + bytesToString(ok6.value)
                            case _: wit.Err[?] =>
                              running = false
                          }
                        }
                        println(s"Body: $body")
                      case _: wit.Err[?] =>
                        println("err 5: invariant violation")
                    }
                  case _: wit.Err[?] =>
                    println("err 4: invariant violation, this should not happen")
                }
              case err3: wit.Err[?] =>
                println(s"err 3: code = ${err3.value}")
            }
          case err2: wit.Err[?] =>
            println(s"err 2: invariant violation ${err2.value}, this should never happen")
        }
      case err: wit.Err[?] =>
        println(s"err 1: code = ${err.value}")
    }

    new wit.Ok(())
  }

}

object Test {
  def bytesToString(bytes: Array[Byte]): String = {
    var res = ""
    bytes.foreach { b =>
      if (b < 128) res = res + b.toChar
      else println(s"skipping byte: $b")
    }
    res
  }

  def stringToBytes(str: String): Array[Byte] = {
    str.collect { case c if c < 128 =>
      c.toByte
    }.toArray
  }
}
