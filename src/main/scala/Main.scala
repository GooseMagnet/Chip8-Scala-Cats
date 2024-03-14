package com.goosemagnet.chip8

import cats.data.StateT
import cats.effect.kernel.{Clock, Temporal}
import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.DurationDouble

object Main extends IOApp {

  private val HZ = 1000.0 / 144

  Thread.sleep(1000)

  override def run(args: List[String]): IO[ExitCode] = {
    val filename = args.head
    for {
      x <- Chip8.loadRom(filename).run(Chip8.create)
      _ <- loop.run(x._1)
    } yield (ExitCode.Success)
  }

  private def loop: StateT[IO, Chip8, Unit] = for {
    start <- StateT.liftF(Clock[IO].monotonic)
    _ <- Chip8.tick
    end <- StateT.liftF(Clock[IO].monotonic)
    _ <- StateT.liftF(Temporal[IO].sleep((HZ - ((end - start).toMillis)).millis))
    _ <- loop
  } yield ()
}
