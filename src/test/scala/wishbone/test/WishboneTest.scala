// See LICENSE for license details.

package examples.test

import Chisel.iotesters._
import wishbone.WishboneSharedBusInterconnection
import Chisel._
import Chisel.testers._
import wishbone._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class ExampleMaster extends Module {
  val io = new WishboneMasterIO()
  io.cycle := Bool(true)
  io.strobe := Bool(true)
}

class ExampleSlave extends Module {
  val io = new WishboneMasterIO().flip()
}

object nMasterIos { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleMaster()).io }
object nSlaveIos  { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleSlave ()).io }

class WishboneSharedBusInterconnectionTester(slaves: Int, masters: Int) extends BasicTester {
  val mastersIO = nMasterIos(masters)
  val slavesIO  = nSlaveIos (slaves)

  WishboneSharedBusInterconnection(
    mastersIO,
    slavesIO
  )
  assert(slavesIO(0).strobe)
  stop()
}

class WishboneSharedBusInterconnectionSpec extends ChiselPropSpec {
  property("Compiles with x masters and y slaves, for x,y in [0,3]"){
    forAll(Gen.choose(0, 3), Gen.choose(0, 3)) {
      (num_masters: Int, num_slaves: Int) =>
      assertTesterPasses{
        new BasicTester {
          WishboneSharedBusInterconnection(
            nMasterIos(num_masters),
            nSlaveIos (num_slaves)
          )
          stop()
        }
      }
    }
  }

  property("Makes some kind of sense"){
    assertTesterPasses{
      new WishboneSharedBusInterconnectionTester(1, 1)
    }
  }
}
