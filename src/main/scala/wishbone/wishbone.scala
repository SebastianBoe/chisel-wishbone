// See LICENSE for license details.

package wishbone

import Chisel._

trait WishboneIp {
  def IO() : WishboneIO
}

trait WishboneSlave  extends WishboneIp {  def inAddressSpace(address: UInt): Bool }
trait WishboneMaster extends WishboneIp { }

class WishboneIO(portSize: Int = 32, granularity: Int = 8) extends Bundle {
/**
 * See wbspec_b4.pdf Chapter 2. Interface Specification.
 * */

  override def cloneType: this.type =
    new WishboneIO(portSize, granularity).asInstanceOf[this.type]

  val address      = UInt(OUTPUT, portSize)
  val dataToSlave  = UInt(OUTPUT , portSize) // DAT_O on master, DAT_I on slave
  val dataToMaster = UInt(INPUT  , portSize) // DAT_I on master, DAT_O on slave
  val writeEnable  = Bool(OUTPUT)
  val select       = Vec( portSize / granularity, Bool() ).asOutput
  val strobe       = Bool(OUTPUT)
  val ack          = Bool(INPUT)
  val cycle        = Bool(OUTPUT)

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

    val masterIos = masters.map(m => m.IO())

    // Use a Counter and a Vec to round-robin select one of the
    // masters
    val (masterIndex, wrap) = Counter(Bool(true), masters.size)
    val masterIo = Vec(masterIos)(masterIndex)

    for (slave <- slaves) {
      // Default to connecting all of the slave's signals to the
      // master
      val slaveIo = slave.IO()
      slaveIo := masterIo

      slaveIo.strobe :=
        masterIo.strobe && slave.inAddressSpace(masterIo.address)
    }

    ////////////////////////////////////////////////////////////////
    //                  Input validation                          //
    ////////////////////////////////////////////////////////////////

    // The masterIo address triggers an address match for at most 1 slave
    val matches : Seq[Bool] = slaves.map(_.inAddressSpace(masterIo.address))
    val num_matches = PopCount(matches)
    Chisel.assert(num_matches <= 1.U, "The address space of slaves must not overlap.")
  }
}
