package com.goosemagnet.chip8

sealed trait OpCode
case object OP_0NNN extends OpCode
case object OP_00E0 extends OpCode
case object OP_00EE extends OpCode
case object OP_1NNN extends OpCode
case object OP_2NNN extends OpCode
case object OP_3XNN extends OpCode
case object OP_4XNN extends OpCode
case object OP_5XY0 extends OpCode
case object OP_6XNN extends OpCode
case object OP_7XNN extends OpCode
case object OP_8XY0 extends OpCode
case object OP_8XY1 extends OpCode
case object OP_8XY2 extends OpCode
case object OP_8XY3 extends OpCode
case object OP_8XY4 extends OpCode
case object OP_8XY5 extends OpCode
case object OP_8XY6 extends OpCode
case object OP_8XY7 extends OpCode
case object OP_8XYE extends OpCode
case object OP_9XY0 extends OpCode
case object OP_ANNN extends OpCode
case object OP_BNNN extends OpCode
case object OP_CXNN extends OpCode
case object OP_DXYN extends OpCode
case object OP_EX9E extends OpCode
case object OP_EXA1 extends OpCode
case object OP_FX07 extends OpCode
case object OP_FX0A extends OpCode
case object OP_FX15 extends OpCode
case object OP_FX18 extends OpCode
case object OP_FX1E extends OpCode
case object OP_FX29 extends OpCode
case object OP_FX33 extends OpCode
case object OP_FX55 extends OpCode
case object OP_FX65 extends OpCode

object OpCode {
  def apply(twoBytes: Char): OpCode = (twoBytes >> 12) match {
    case 0x0 => parse0(twoBytes)
    case 0x1 => OP_1NNN
    case 0x2 => OP_2NNN
    case 0x3 => OP_3XNN
    case 0x4 => OP_4XNN
    case 0x5 => OP_5XY0
    case 0x6 => OP_6XNN
    case 0x7 => OP_7XNN
    case 0x8 => parse8(twoBytes)
    case 0x9 => OP_9XY0
    case 0xA => OP_ANNN
    case 0xB => OP_BNNN
    case 0xC => OP_CXNN
    case 0xD => OP_DXYN
    case 0xE => parseE(twoBytes)
    case 0xF => parseF(twoBytes)
  }

  private def parse0(opCode: Char): OpCode = opCode match {
    case 0x00E0 => OP_00E0
    case 0x00EE => OP_00EE
  }

  private def parse8(opCode: Char): OpCode = opCode & 0x000F match {
    case 0x0 => OP_8XY0
    case 0x1 => OP_8XY1
    case 0x2 => OP_8XY2
    case 0x3 => OP_8XY3
    case 0x4 => OP_8XY4
    case 0x5 => OP_8XY5
    case 0x6 => OP_8XY6
    case 0x7 => OP_8XY7
    case 0xE => OP_8XYE
  }

  private def parseE(opCode: Char): OpCode = opCode & 0x00FF match {
    case 0x9E => OP_EX9E
    case 0xA1 => OP_EXA1
  }

  private def parseF(opCode: Char): OpCode = opCode & 0x00FF match {
    case 0x07 => OP_FX07
    case 0x0A => OP_FX0A
    case 0x15 => OP_FX15
    case 0x18 => OP_FX18
    case 0x1E => OP_FX1E
    case 0x29 => OP_FX29
    case 0x33 => OP_FX33
    case 0x55 => OP_FX55
    case 0x65 => OP_FX65;
  }
}