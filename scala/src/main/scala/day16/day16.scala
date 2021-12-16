package day16

import scala.annotation.tailrec
import scala.io.Source

type Packet = Literal | Operation
case class Literal(version: Int, data: Long)
case class Operation(version: Int, opType: Int, data: List[Packet] = List())

type Result = (Packet, String)

def parseHeader(packet: String): Result = (Integer.parseInt(packet.take(3), 2), Integer.parseInt(packet.slice(3, 6), 2)) match {
  case (version, 4) => parseLiteral(version, packet.drop(6))
  case (version, opType) => parseOp(version, opType, packet.drop(6))
}

def parseLiteral(version: Int, rest: String): Result =
  val groupOfFives = rest.grouped(5).toList
  val bitsFromFirstZero = groupOfFives.dropWhile(_.head == '1')
  val number = bitsToNumber(groupOfFives.takeWhile(_.head == '1') ++ bitsFromFirstZero.take(1))
  (Literal(version, number), bitsFromFirstZero.drop(1).mkString)

def bitsToNumber(list: List[String]): Long =
  val num = list.map(_.drop(1)).mkString
  BigInt(num, 2).toLong

def parseOp(version: Int, opType: Int, rest: String): Result = rest.head match {
  case '0' =>
    val (num, subPacketBits) = rest.tail.splitAt(15)
    val subPacketLength = Integer.parseInt(num, 2)
    val (subPackets, remainingBits) = subPacketBits.splitAt(subPacketLength)
    val (packets, rest2) = parseUntilEmpty(str = subPackets)
    (Operation(version, opType, packets.reverse), remainingBits)
  case '1' =>
    val (num, subPacketBits) = rest.tail.splitAt(11)
    val subPacketNumber = Integer.parseInt(num, 2)
    val (subPackets, remainingBits) = (0 until subPacketNumber).foldLeft((List(): List[Packet], subPacketBits))((acc, foo) => getPacket(acc._1, acc._2))
    (Operation(version, opType, subPackets.reverse), remainingBits)
  case _ => throw RuntimeException(s"Can't parse ${rest.head}")
}

def getPacket(list: List[Packet], str: String): (List[Packet], String) =
  val (packet, rest) = parseHeader(str)
  (packet::list, rest)

@tailrec
def parseUntilEmpty(packets: List[Packet] = List(), str: String): (List[Packet], String) = str match {
  case "" => (packets, "")
  case _ =>
    val (packet, rest) = parseHeader(str)
    parseUntilEmpty(packet::packets, rest)
}

def toBinary(string: String): String =
  string.flatMap(toBinary)

def toBinary(c: Char): String = c match {
  case '0' => "0000"
  case '1' => "0001"
  case '2' => "0010"
  case '3' => "0011"
  case '4' => "0100"
  case '5' => "0101"
  case '6' => "0110"
  case '7' => "0111"
  case '8' => "1000"
  case '9' => "1001"
  case 'A' => "1010"
  case 'B' => "1011"
  case 'C' => "1100"
  case 'D' => "1101"
  case 'E' => "1110"
  case 'F' => "1111"
}

def getVersionSum(packet: Packet): Int = packet match {
  case Literal(version, _) => version
  case Operation(version, _, packages) => version + packages.foldLeft(0)((acc, p) => acc + getVersionSum(p))
}

def doCalculation(packet: Packet): Long = packet match {
  case Literal(_, num) => num
  case Operation(_, 0, packets) => packets.map(doCalculation).sum
  case Operation(_, 1, packets) => packets.map(doCalculation).product
  case Operation(_, 2, packets) => packets.map(doCalculation).min
  case Operation(_, 3, packets) => packets.map(doCalculation).max
  case Operation(_, 5, List(p1, p2)) => if doCalculation(p1) > doCalculation(p2) then 1 else 0
  case Operation(_, 6, List(p1, p2)) => if doCalculation(p1) < doCalculation(p2) then 1 else 0
  case Operation(_, 7, List(p1, p2)) => if doCalculation(p1) == doCalculation(p2) then 1 else 0
  case _ => throw RuntimeException(s"Can't parse $packet")
}

@main
def main(): Unit =
  val line = Source.fromResource("day16.txt").getLines().filterNot(_.isBlank).toList.head

  val (packets, _) = parseHeader(toBinary(line))
  println(s"version: ${getVersionSum(packets)}")
  println(s"calculation: ${doCalculation(packets)}")


val oneLiteral = "D2FE28" // v 6
val exTwoPackets = "38006F45291200"
val opOpOpLiteral = "8A004A801A8002F478"
val exv12 = "620080001611562C8802118E34"
val exv23 = "C0015000016115A2E0802F182340"
val exv31 = "A0016C880162017C3686B18A3D4780"

val p2v3 = "C200B40A82"
val p2v54 = "04005AC33890"
val p2v7 = "880086C3E88112"
val p2v9 = "CE00C43D881120"
val p2v1 = "D8005AC2A8F0"
val p2v0 = "F600BC2D8F"
val p2v00 = "9C005AC2F8F0"
val p2v01 = "9C0141080250320F1802104A08"

