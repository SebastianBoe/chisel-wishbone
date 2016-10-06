// See LICENSE for license details.

package examples.test

import Chisel.iotesters._
import wishbone.WishboneSharedBusInterconnection
import Chisel._
import Chisel.testers._
import wishbone._

class ExampleMaster extends Module with WishboneMaster {
  val io = new WishboneIO()

  def IO() = io // TODO: How to get rid of this boilerplate?

  io.cycle := Bool(true)
  io.strobe := Bool(true)
}

class ExampleSlave(i: Int) extends Module with WishboneSlave {
  val io = new WishboneIO().flip()

  def IO() = io
  def inAddressSpace(address: UInt) = address === UInt(i)
}

object nMasters { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleMaster( )) }
object nSlaves  { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleSlave (i)) }

class WishboneSharedBusInterconnectionSpec extends ChiselPropSpec {
  property("Compiles with x masters and y slaves, for x,y in [0,3]"){
      assertTesterPasses{
        new BasicTester {
          for(num_masters <- 0 to 3; num_slaves <- 0 to 3) {
            WishboneSharedBusInterconnection(
              nMasters(num_masters),
              nSlaves (num_slaves)
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
        val mastersIO = nMasters(1)
        val slavesIO  = nSlaves (1)

        WishboneSharedBusInterconnection(
          mastersIO,
          slavesIO
        )
        Chisel.assert(slavesIO(0).io.strobe)
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

        val slaves = nSlaves(3)
        WishboneSharedBusInterconnection(
          Module(new Module with WishboneMaster { val io = new WishboneIO(); def IO() = io }),
          slaves
        )
        Chisel.assert(slaves(0).io.strobe === Bool(false))
      }
    }
  }

  property("Only one slave is selected at a time"){
    assertTesterPasses{
      new BasicTester{
        val slaves = nSlaves(2)
        WishboneSharedBusInterconnection(
          nMasters(3),
          slaves
        )
        Chisel.assert(slaves(0).io.strobe ^ slaves(1).io.strobe)
        stop()
      }
    }
  }

  property("A slave only receives accesses for it's address range"){
    assertTesterPasses{
      new BasicTester
      {
        val slave = Module(
          new Module with WishboneSlave {
            val io = new WishboneIO().flip()
            def IO() = io
            def inAddressSpace(address: UInt) : Bool = address === UInt(4)
          }
        )

        val master = Module( new Module with WishboneMaster {
          val io = new WishboneIO()
          def IO = io
          io.cycle := Bool(true)
          io.strobe := Bool(true)
          val (cnt, done) = Counter(!reset, 20)
          io.address := cnt
        })

        WishboneSharedBusInterconnection(
          master,
          slave
        )

        Chisel.assert(
          slave.io.strobe === (master.io.address === UInt(4))
        )

        when(master.io.address === UInt(19)) { stop() }
      }
    }
  }
}
