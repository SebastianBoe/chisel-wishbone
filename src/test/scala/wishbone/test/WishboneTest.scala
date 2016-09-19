// See LICENSE for license details.

package examples.test

import Chisel.iotesters._
import wishbone.WishboneSharedBusInterconnection
import Chisel._
import Chisel.testers._
import wishbone._

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
          Chisel.assert(Bool(false) === Bool(false))
          stop()
        }
    }
  }

  property("A slave receives a strobe when all masters make requests"){
    assertTesterPasses{
      new BasicTester {
        val mastersIO = nMasterIos(1)
        val slavesIO  = nSlaveIos (1)

        WishboneSharedBusInterconnection(
          mastersIO,
          slavesIO
        )
        Chisel.assert(slavesIO(0).strobe)
        stop()
      }
    }
  }

  property("Slave 0 does not receive a strobe when none of the masters are strobing"){
    assertTesterPasses{
      new BasicTester{
        val max = 3
        val cnt = Counter(max)
        when(Bool(true)) { cnt.inc() }
        when(cnt.value === UInt(max-1)) {
          stop()
        }

        val slaves = nSlaveIos(3)
        WishboneSharedBusInterconnection(
          Module(new Module { val io = new WishboneMasterIO() }).io,
          slaves
        )
        Chisel.assert(slaves(0).strobe === Bool(false))
      }
    }
  }

  property("Only one slave is selected at a time"){
    assertTesterPasses{
      new BasicTester{
        val slaves = nSlaveIos(2)
        WishboneSharedBusInterconnection(
          nMasterIos(3),
          slaves
        )
        Chisel.assert(slaves(0).strobe ^ slaves(1).strobe)
        stop()
      }
    }
  }

}
