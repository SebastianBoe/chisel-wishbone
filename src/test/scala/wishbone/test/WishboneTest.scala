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

class ATester extends BasicTester {
  val slaves = nSlaveIos(3)
  WishboneSharedBusInterconnection(
    new Module { val io = new WishboneMasterIO() }.io,
    slaves
  )
  assert(slaves(0).strobe)
  stop()
}

class WishboneSharedBusInterconnectionSpec extends ChiselPropSpec {
  property("Compiles with x masters and y slaves, for x,y in [0,3]"){
      assertTesterPasses{
        new BasicTester {
          for(num_masters <- 0 to 3; num_slaves <- 0 to 3) {
            WishboneSharedBusInterconnection(
              nMasterIos(num_masters),
              nSlaveIos (num_slaves)
            )
          }
          stop()
        }
    }
  }

  property("A slave receives a strobe when all masters make requests"){
    assertTesterPasses{
      new WishboneSharedBusInterconnectionTester(1, 1)
    }
  }

  property("Slave 0 does not receive a strobe when none of the masters are strobing"){
    assertTesterPasses{
      new BasicTester {
        val slaves = nSlaveIos(3)
        WishboneSharedBusInterconnection(
          new Module { val io = new WishboneMasterIO() }.io,
          slaves
        )
        assert(slaves(0).strobe)
        stop()
      }
    }
  }
}
