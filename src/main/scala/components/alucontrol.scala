// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for 64-bit R-type
 *                          . 2 for 64-bit I-type
 *                          . 3 for 32-bit R-type
 *                          . 4 for 32-bit I-type
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  // R-type 64 bit operations
  when(io.aluop === "b001".U) {

    when(io.funct7 === "b0000000".U) {
      when(io.funct3 === "b000".U) { io.operation := "b00001".U } // add 64
      .elsewhen(io.funct3 === "b001".U) { io.operation := "b10010".U } // sll 64
      .elsewhen(io.funct3 === "b010".U) { io.operation := "b10110".U } // slt 64
      .elsewhen(io.funct3 === "b011".U) { io.operation := "b10111".U } // sltu 64
      .elsewhen(io.funct3 === "b100".U) { io.operation := "b01111".U } // xor 64
      .elsewhen(io.funct3 === "b101".U) { io.operation := "b10100".U } // srl 64
      .elsewhen(io.funct3 === "b110".U) { io.operation := "b01110".U } // or 64
      .elsewhen(io.funct3 === "b111".U) { io.operation := "b01101".U } // and 64
    }

    .elsewhen(io.funct7 === "b0100000".U) {
      when(io.funct3 === "b000".U) { io.operation := "b00100".U } // sub 64
      .elsewhen(io.funct3 === "b101".U) { io.operation := "b10000".U } // sra 64
    }

    .elsewhen(io.funct7 === "b0000001".U) {
      when(io.funct3 === "b000".U) { io.operation := "b00110".U } // mul 64
      .elsewhen(io.funct3 === "b001".U) { io.operation := "b00111".U } // mulh 64
      .elsewhen(io.funct3 === "b010".U) { io.operation := "b11000".U } // mulhsu 64
      .elsewhen(io.funct3 === "b011".U) { io.operation := "b01000".U } // mulhu 64
      .elsewhen(io.funct3 === "b100".U) { io.operation := "b01011".U } // div 64
      .elsewhen(io.funct3 === "b101".U) { io.operation := "b01010".U } // divu 64
      .elsewhen(io.funct3 === "b110".U) { io.operation := "b11100".U } // rem 64
      .elsewhen(io.funct3 === "b111".U) { io.operation := "b11011".U } //remu 64
    }
  }

  // 32 bit operations
  when(io.aluop === "b011".U) {

    when(io.funct7 === "b0000000".U) {
      when(io.funct3 === "b000".U) { io.operation := "b00000".U } // addw 32
      .elsewhen(io.funct3 === "b001".U) {io.operation := "b10011".U } // sllw 32
      .elsewhen(io.funct3 === "b101".U) {io.operation := "b10101".U } // srlw 32
    }

    .elsewhen(io.funct7 === "b0100000".U) {
      when(io.funct3 === "b000".U) { io.operation := "b00010".U } // subw 32
      .elsewhen(io.funct3 === "b101".U) { io.operation := "b10001".U } // sraw 32
    } 

    .elsewhen(io.funct7 === "b0000001".U) {
      when(io.funct3 === "b000".U) { io.operation := "b00101".U } // mulw 32
      .elsewhen(io.funct3 === "b100".U) { io.operation := "b01001".U } // divw 32
      .elsewhen(io.funct3 === "b101".U) { io.operation := "b01100".U } // divuw 32
      .elsewhen(io.funct3 === "b110".U) { io.operation := "b11010".U } // remw 32
      .elsewhen(io.funct3 === "b111".U) { io.operation := "b11001".U } // remuw 32
    }
  }
}
