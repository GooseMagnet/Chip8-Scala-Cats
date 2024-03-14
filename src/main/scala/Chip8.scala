package com.goosemagnet.chip8

import cats.data.StateT
import cats.effect.IO

import java.nio.file.{Files, Path}
import java.util.concurrent.ThreadLocalRandom
import scala.collection.immutable.ArraySeq

final case class Chip8 private (
  registers: ArraySeq[Byte] = ArraySeq().appendedAll(Array.ofDim(16)),
  index: Char = 0,
  pc: Char = 0x200,
  memory: ArraySeq[Byte] = ArraySeq().appendedAll(Array.ofDim(4096)),
  screen: Screen = Screen.create(),
  dt: Byte = 0,
  st: Byte = 0,
  sp: Byte = 0,
  stack: ArraySeq[Char] = ArraySeq().appendedAll(Array.ofDim(16))
)

object Chip8 {

  def create: Chip8 = {
    Chip8(memory = initMemory)
  }

  private def initMemory: ArraySeq[Byte] = {
    val fonts: Array[Byte] = Array(
      0xf0, 0x90, 0x90, 0x90, 0xf0, // 0
      0x20, 0x60, 0x20, 0x20, 0x70, // 1
      0xf0, 0x10, 0xf0, 0x80, 0xf0, // 2
      0xf0, 0x10, 0xf0, 0x10, 0xf0, // 3
      0x90, 0x90, 0xf0, 0x10, 0x10, // 4
      0xf0, 0x80, 0xf0, 0x10, 0xf0, // 5
      0xf0, 0x80, 0xf0, 0x90, 0xf0, // 6
      0xf0, 0x10, 0x20, 0x40, 0x40, // 7
      0xf0, 0x90, 0xf0, 0x90, 0xf0, // 8
      0xf0, 0x90, 0xf0, 0x10, 0xf0, // 9
      0xf0, 0x90, 0xf0, 0x90, 0x90, // A
      0xe0, 0x90, 0xe0, 0x90, 0xe0, // B
      0xf0, 0x80, 0x80, 0x80, 0xf0, // C
      0xe0, 0x90, 0x90, 0x90, 0xe0, // D
      0xf0, 0x80, 0xf0, 0x80, 0xf0, // E
      0xf0, 0x80, 0xf0, 0x80, 0x80 // F
    ).map(_.toByte)
    val memory = Array.ofDim[Byte](4096)
    fonts.copyToArray(memory, 0)
    ArraySeq().appendedAll(memory)
  }

  def loadRom(fileName: String): StateT[IO, Chip8, Unit] = StateT { c8 =>
    for {
      _ <- IO.println(s"Loading: $fileName")
      bytes <- IO(Files.readAllBytes(Path.of(fileName)))
      byteArray = c8.memory.toArray
      _ = bytes.copyToArray(byteArray, 0x200)
    } yield (c8.copy(memory = ArraySeq().appendedAll(byteArray)), ())
  }

  def tick: StateT[IO, Chip8, Unit] = for {
    highByte <- getNextByte
    lowByte <- getNextByte
    twoBytes = ((highByte << 8) | (lowByte & 0xff)).toChar
    opCode = OpCode(twoBytes)
    _ <- StateT.liftF(IO.println(f"$twoBytes%x->$opCode"))
    _ <- execute(opCode, twoBytes)
    c8 <- StateT.get[IO, Chip8]
    _ = c8.screen.tick()
    _ <- decrementDelayAndSound
  } yield ()

  private def execute(opCode: OpCode, twoBytes: Char): StateT[IO, Chip8, Unit] = opCode match {
    case OP_0NNN => StateT.liftF(IO())
    case OP_00E0 => handleClearScreen
    case OP_00EE => handleRet
    case OP_1NNN => handleJumpAddr(twoBytes)
    case OP_2NNN => handleCallAddr(twoBytes)
    case OP_3XNN => handleSkipVxKk(twoBytes, eq = true)
    case OP_4XNN => handleSkipVxKk(twoBytes, eq = false)
    case OP_5XY0 => handleSkipVxVy(twoBytes, eq = true)
    case OP_6XNN => handleLoadVxKk(twoBytes)
    case OP_7XNN => handleAddVxKk(twoBytes)
    case OP_8XY0 => handleLoadVxVy(twoBytes)
    case OP_8XY1 => handleOrVxVy(twoBytes)
    case OP_8XY2 => handleAndVxVy(twoBytes)
    case OP_8XY3 => handleXorVxVy(twoBytes)
    case OP_8XY4 => handleAddVxVy(twoBytes)
    case OP_8XY5 => handleSubVxVy(twoBytes)
    case OP_8XY6 => handleShrVx(twoBytes)
    case OP_8XY7 => handleSubnVxVy(twoBytes)
    case OP_8XYE => handleShlVx(twoBytes)
    case OP_9XY0 => handleSkipVxVy(twoBytes, eq = false)
    case OP_ANNN => handleLoadIAddr(twoBytes)
    case OP_BNNN => handleJumpV0Addr(twoBytes)
    case OP_CXNN => handleRndAndKk(twoBytes)
    case OP_DXYN => handleDrawVxVyNibble(twoBytes)
    case OP_EX9E => handleSkipVx(twoBytes, pressed = true)
    case OP_EXA1 => handleSkipVx(twoBytes, pressed = false)
    case OP_FX07 => handleLoadVxDelayTimer(twoBytes)
    case OP_FX0A => handleLoadVxKey(twoBytes)
    case OP_FX15 => handleLoadDelayTimerVx(twoBytes)
    case OP_FX18 => handleLoadSoundTimerVx(twoBytes)
    case OP_FX1E => handleAddIVx(twoBytes)
    case OP_FX29 => handleLoadSprite(twoBytes)
    case OP_FX33 => handleLoadBinaryCodedDecimalVx(twoBytes)
    case OP_FX55 => handleLoadRegistersIntoMemory(twoBytes)
    case OP_FX65 => handleLoadMemoryIntoRegisters(twoBytes)
  }

  // INSTRUCTION HANDLERS
  private def handleClearScreen: StateT[IO, Chip8, Unit] = StateT.inspect(c8 => c8.screen.clear())
  private def handleRet: StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(pc = c8.stack(c8.sp), sp = (c8.sp - 1).toByte), ())
  }
  private def handleJumpAddr(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(pc = getAddr(twoBytes)), ())
  }
  private def handleCallAddr(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val newSp = (c8.sp + 1).toByte
    IO(c8.copy(sp = newSp, stack = c8.stack.updated(newSp, c8.pc), pc = getAddr(twoBytes)), ())
  }
  private def handleSkipVxKk(twoBytes: Char, eq: Boolean): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val Vx = c8.registers(getX(twoBytes))
    val lowByte = getLowByte(twoBytes)
    if (eq == (Vx == lowByte)) skipNextInstruction.run(c8) else IO(c8, ())
  }
  private def handleSkipVxVy(twoBytes: Char, eq: Boolean): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val Vx = c8.registers(getX(twoBytes))
    val Vy = c8.registers(getY(twoBytes))
    if (eq == (Vx == Vy)) skipNextInstruction.run(c8) else IO(c8, ())
  }
  private def handleLoadVxKk(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val lowByte = getLowByte(twoBytes)
    IO((c8.copy(registers = c8.registers.updated(x, lowByte)), ()))
  }
  private def handleAddVxKk(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val lowByte = getLowByte(twoBytes)
    val newValue = c8.registers(x) + lowByte
    IO(c8.copy(registers = c8.registers.updated(x, newValue.toByte)), ())
  }
  private def handleLoadVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val y = getY(twoBytes)
    IO((c8.copy(registers = c8.registers.updated(x, c8.registers(y))), ()))
  }
  private def handleOrVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = alu(twoBytes, _ | _)
  private def handleAndVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = alu(twoBytes, _ & _)
  private def handleXorVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = alu(twoBytes, _ ^ _)
  private def handleAddVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val y = getY(twoBytes)
    val sum = ((c8.registers(x) + c8.registers(y)) & 0x00ff).toChar
    val carry = if (sum > 0xff) 1 else 0
    IO(c8.copy(registers = c8.registers.updated(x, sum.toByte).updated(0xf, carry.toByte)), ())
  }
  private def handleSubVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val y = getY(twoBytes)
    val borrow: Byte = if ((0xFF & c8.registers(x)) > (0xFF & c8.registers(y))) 1 else 0
    val diff = (c8.registers(x) - c8.registers(y)) & 0x00ff
    IO(c8.copy(registers = c8.registers.updated(x, diff.toByte).updated(0xf, borrow)), ())
  }
  private def handleShrVx(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val shrVx = c8.registers(x) >> 1
    IO(c8.copy(registers = c8.registers.updated(x, shrVx.toByte).updated(0xf, (c8.registers(0xF) & 1).toByte)), ())
  }
  private def handleSubnVxVy(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val y = getY(twoBytes)
    val borrow: Byte = if (c8.registers(y) > c8.registers(x)) 1 else 0
    val diff = (c8.registers(y) - c8.registers(x)).toByte
    IO(c8.copy(registers = c8.registers.updated(x, diff).updated(0xf, borrow)), ())
  }
  private def handleShlVx(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val msb = ((c8.registers(x) & Integer.parseInt("10000000", 2)) >> 7).toByte
    IO(c8.copy(registers = c8.registers.updated(x, (c8.registers(x) << 1).toByte).updated(0xf, msb)), ())
  }
  private def handleLoadIAddr(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(index = getAddr(twoBytes)), ())
  }
  private def handleJumpV0Addr(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(pc = (c8.registers(0) + getAddr(twoBytes)).toChar), ())
  }
  private def handleRndAndKk(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val rnd = ThreadLocalRandom.current().nextInt(256)
    val lowByte = getLowByte(twoBytes)
    val x = getX(twoBytes)
    IO(c8.copy(registers = c8.registers.updated(x, (rnd & lowByte).toByte)), ())
  }
  private def handleDrawVxVyNibble(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val height = twoBytes & 0x000f
    val Vx = c8.registers(getX(twoBytes))
    val Vy = c8.registers(getY(twoBytes))

    var pixelCollision = false

    for (i <- 0 until height if Vy + i < c8.screen.getHeight && Vy + i >= 0) {
      val sprite = c8.memory(c8.index + i)
      for (j <- 0 until 8 if Vx + j < c8.screen.getWidth && Vx + j >= 0) {
        val curY = Vy + i
        val curX = Vx + j
        val spritePixelOn = (sprite & Integer.parseInt("10000000", 2) >> j) > 0
        val screenPixelOn = c8.screen.getPixel(curX, curY)
        c8.screen.setPixel(curX, curY, spritePixelOn ^ screenPixelOn)
        pixelCollision = spritePixelOn && screenPixelOn
      }
    }

    val collided = if (pixelCollision) 1 else 0
    IO(c8.copy(registers = c8.registers.updated(0xf, collided.toByte)), ())
  }
  private def handleSkipVx(twoBytes: Char, pressed: Boolean): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val Vx = c8.registers(getX(twoBytes))
    val keyIsPressed = c8.screen.getKey(Vx)
    if (pressed == keyIsPressed) skipNextInstruction.run(c8) else IO(c8, ())
  }
  private def handleLoadVxDelayTimer(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(registers = c8.registers.updated(getX(twoBytes), c8.dt)), ())
  }
  private def handleLoadVxKey(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    for (i <- 0 until c8.screen.getNumberOfKeys) {
      if (c8.screen.getKey(i)) {
        IO(c8.copy(c8.registers.updated(getX(twoBytes), i.toByte)), ())
      }
    }
    burnCpuCycle.run(c8)
  }
  private def handleLoadDelayTimerVx(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(dt = c8.registers(getX(twoBytes))), ())
  }
  private def handleLoadSoundTimerVx(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(st = c8.registers(getX(twoBytes))), ())
  }
  private def handleAddIVx(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    IO(c8.copy(index = (c8.index + c8.registers(x)).toChar), ())
  }
  private def handleLoadSprite(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val Vx = c8.registers(getX(twoBytes))
    IO(c8.copy(index = (Vx * 5).toChar), ())
  }
  private def handleLoadBinaryCodedDecimalVx(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val bcd = c8.registers(getX(twoBytes)) & 0xFF
    val first = ((bcd / 100) % 10).toByte
    val second = ((bcd / 10) % 10).toByte
    val third = (bcd % 10).toByte
    val idx = c8.index
    IO(
      c8.copy(memory =
        c8.memory
          .updated(idx + 2, third)
          .updated(idx + 1, second)
          .updated(idx, first)
      ),
      ()
    )
  }
  private def handleLoadRegistersIntoMemory(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    var memory = c8.memory
    for (i <- 0 to x) {
      memory = memory.updated(c8.index + i, c8.registers(i))
    }
    IO(c8.copy(memory = memory), ())
  }
  private def handleLoadMemoryIntoRegisters(twoBytes: Char): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    var registers = c8.registers
    for (i <- 0 to x) {
      registers = registers.updated(i, c8.memory(c8.index + i))
    }
    IO(c8.copy(registers = registers), ())
  }

  // HELPERS
  private def getNextByte: StateT[IO, Chip8, Byte] = StateT(c8 => {
    IO(c8.copy(pc = (c8.pc + 1).toChar), c8.memory(c8.pc))
  })
  private def decrementDelayAndSound: StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(c8.copy(dt = 0.max((c8.dt - 1)).toByte, st = 0.max((c8.st - 1)).toByte), ())
  }
  private def getAddr(twoBytes: Char): Char = (twoBytes & 0x0fff).toChar
  private def getX(twoBytes: Char): Byte = ((twoBytes & 0x0f00) >> 8).toByte
  private def getY(twoBytes: Char): Byte = ((twoBytes & 0x00f0) >> 4).toByte
  private def getLowByte(twoBytes: Char): Byte = (twoBytes & 0x00ff).toByte
  private def skipNextInstruction: StateT[IO, Chip8, Unit] = StateT(c8 => IO(c8.copy(pc = (c8.pc + 2).toChar), ()))
  private def alu(twoBytes: Char, fn: (Int, Int) => Int): StateT[IO, Chip8, Unit] = StateT { c8 =>
    val x = getX(twoBytes)
    val y = getY(twoBytes)
    IO((c8.copy(registers = c8.registers.updated(x, fn(c8.registers(x), c8.registers(y)).toByte)), ()))
  }
  private def burnCpuCycle: StateT[IO, Chip8, Unit] = StateT { c8 =>
    IO(
      c8.copy(pc = (c8.pc - 2).toChar, dt = (c8.dt + 1).toByte, st = (c8.st + 1).toByte),
      ()
    )
  }
}
