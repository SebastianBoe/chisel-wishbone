// See LICENSE for license details.

package wishbone

import Chisel._

class WishboneMasterIO(portSize: Int = 32, granularity: Int = 8) extends Bundle {
/**
 * See wbspec_b4.pdf Chapter 2. Interface Specification.
 * */

  override def cloneType: this.type =
    new WishboneMasterIO(portSize, granularity).asInstanceOf[this.type]

  val address      = UInt(OUTPUT, portSize)
  val dataToSlave  = UInt(OUTPUT , portSize) // DAT_O on master, DAT_I on slave
  val dataToMaster = UInt(INPUT  , portSize) // DAT_I on master, DAT_O on slave
  val writeEnable  = Bool(OUTPUT)
  val select       = Bool(OUTPUT)
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
  def apply(master: WishboneMasterIO, slaves : Seq[WishboneMasterIO]){ apply(List(master), slaves) }
  def apply(masters: Seq[WishboneMasterIO], slave : WishboneMasterIO){ apply(masters, List(slave)) }
  def apply(master: WishboneMasterIO, slave : WishboneMasterIO){ apply(List(master), List(slave)) }

  def apply(
    masters: Seq[WishboneMasterIO],
    slaves : Seq[WishboneMasterIO]
  ) {
    if(masters.isEmpty || slaves.isEmpty){
      return
    }
    slaves.foreach(
      _.strobe := Cat(
        masters.map(x => x.strobe)
      ).orR()
    )
  }
}
