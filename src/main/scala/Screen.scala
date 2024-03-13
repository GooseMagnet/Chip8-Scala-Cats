package com.goosemagnet.chip8

import Screen.SCALE

import java.awt.event.{KeyEvent, KeyListener}
import java.awt._

class Screen private (canvas: Canvas, private var pixels: Array[Array[Boolean]], keys: Array[Boolean]) {
  canvas.requestFocus()
  lazy val getHeight: Int = pixels.length
  lazy val getWidth: Int = pixels(0).length
  lazy val getNumberOfKeys: Int = keys.length
  def getPixel(x: Int, y: Int): Boolean = pixels(y)(x)
  def setPixel(x: Int, y: Int, isOn: Boolean): Unit = pixels(y)(x) = isOn
  def getKey(key: Int): Boolean = keys(key)
  def clear(): Unit = pixels = Array.ofDim[Boolean](getHeight, getWidth)
  def tick(): Unit = {
    val graphics = canvas.getGraphics
    graphics.setColor(Color.BLACK)
    graphics.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
    graphics.setColor(Color.WHITE)
    for (i <- 0 until getHeight; j <- 0 until getWidth) {
      if (pixels(i)(j)) {
        graphics.fillRect(SCALE * j, SCALE * i, Math.max(1, SCALE - 1), Math.max(1, SCALE - 1))
      }
    }
    Toolkit.getDefaultToolkit.sync()
  }
}

object Screen {
  private val SCALE = 20
  private val PIX_H = 32
  private val PIX_W = 64
  private val HEIGHT = PIX_H * SCALE
  private val WIDTH = PIX_W * SCALE
  private val SIZE = new Dimension(WIDTH, HEIGHT)

  def create(): Screen = {
    val keys: Array[Boolean] = new Array[Boolean](16)
    val canvas = createCanvas(SIZE, new Chip8KeyListener(keys))

    val frame: Frame = new Frame("Chip8")
    frame.setResizable(false)
    frame.setSize(SIZE)
    frame.setLocationRelativeTo(null)
    frame.add(canvas)
    frame.pack()
    frame.setVisible(true)
    new Screen(canvas, Array.ofDim[Boolean](PIX_H, PIX_W), keys)
  }

  private def createCanvas(dimension: Dimension, keyListener: KeyListener): Canvas = {
    val canvas: Canvas = new Canvas()
    canvas.setBackground(Color.BLACK)
    canvas.setSize(dimension)
    canvas.addKeyListener(keyListener)
    canvas
  }

  private class Chip8KeyListener(val keys: Array[Boolean]) extends KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {}
    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_ESCAPE) System.exit(0)
      interact(e, isOn = true)
    }
    override def keyReleased(e: KeyEvent): Unit = interact(e, isOn = false)
    private def interact(e: KeyEvent, isOn: Boolean): Unit = {
      if (e.getKeyCode >= '0' && e.getKeyCode <= '9') keys(e.getKeyCode - '0') = isOn
      else keys(e.getKeyCode - 'A') = isOn
    }
  }
}
