// See LICENSE for license details.

package wishbone

import chisel3._
import chisel3.util._
import chisel3.testers._
import chisel3.core.SeqUtils

trait WishboneIp {
  def get_io() : WishboneIO
}

trait WishboneSlave  extends WishboneIp {  def inAddressSpace(address: UInt): Bool }
trait WishboneMaster extends WishboneIp { }

class WishboneIO(portSize: Int = 32, granularity: Int = 8) extends Bundle {
/**
 * See wbspec_b4.pdf Chapter 2. Interface Specification.
 * */

  override def cloneType: this.type =
    new WishboneIO(portSize, granularity).asInstanceOf[this.type]

  val address      = Output(UInt.width(portSize))
  val dataToSlave  = Output(UInt.width(portSize)) // DAT_O on master, DAT_I on slave
  val dataToMaster = Input (UInt.width(portSize)) // DAT_I on master, DAT_O on slave
  val writeEnable  = Output(Bool())
  val select       = Output(Vec( portSize / granularity, Bool() ))
  val strobe       = Output(Bool())
  val ack          = Input (Bool())
  val cycle        = Output(Bool())

  // TODO: Understand and then support tag's
  // val tgd = UInt(OUTPUT, tagSize)
  // val tga = UInt(OUTPUT, tagSize)
  // val tgc = UInt(OUTPUT, tagSize)
}

object WishboneSharedBusInterconnection
{
  // Convenience functions to be able to call this function with either a list or an element
  def apply(master  : WishboneMaster      , slaves : Seq[WishboneSlave]){ apply(List(master), slaves     ) }
  def apply(masters : Seq[WishboneMaster] , slave  : WishboneSlave     ){ apply(masters     , List(slave)) }
  def apply(master  : WishboneMaster      , slave  : WishboneSlave     ){ apply(List(master), List(slave)) }

  def apply(
    masters: Seq[WishboneMaster],
    slaves : Seq[WishboneSlave]
  ) : Unit = {
    if(masters.isEmpty || slaves.isEmpty){
      // We need at least one master and at least one slave to make a
      // shared bus.
      return
    }

    val masterIos = masters.map(m => m.get_io())

    // Use a Counter and a Vec to round-robin select one of the
    // masters
    val (masterIndex, wrap) = Counter(Bool(true), masters.size)
    val bus = Vec(masterIos)(masterIndex)

    for (slave <- slaves) {
      // Default to connecting all of the slave's signals to the bus
      val slaveIo = slave.get_io()
      slaveIo <> bus

      slaveIo.strobe :=
        bus.strobe && slave.inAddressSpace(bus.address)
    }

    val slaveBus = Mux1H(
      for ( slave <- slaves)
        yield (
        slave.inAddressSpace(bus.address),
        slave.get_io()
      )
    )

    for ( (masterIo, i) <- masterIos zipWithIndex){
      val master_has_been_granted = UInt(i) === masterIndex
      masterIo.ack := Mux(
        master_has_been_granted,
        slaveBus.ack,
        Bool(false)
      )
    }

    ////////////////////////////////////////////////////////////////
    //                  Input validation                          //
    ////////////////////////////////////////////////////////////////

    // The bus address triggers an address match for at most 1 slave
    val matches : Seq[Bool] = slaves.map(_.inAddressSpace(bus.address))
    val num_matches = PopCount(matches)
    Chisel.assert(num_matches <= 1.U, "The address space of slaves must not overlap.")
  }
}
