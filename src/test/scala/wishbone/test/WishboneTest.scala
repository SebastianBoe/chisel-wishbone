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

  io.cycle   := Bool(true)
  io.strobe  := Bool(true)
  io.address := UInt(0)

  io.dataToSlave := UInt(1)

  val myVec = Wire( Vec(32 / 8, Bool()) )
  myVec foreach (_ := Bool(true))

  io.select      := myVec
  io.writeEnable := Bool(false)
}

class ExampleSlave(i: Int) extends Module with WishboneSlave {
  val io = new WishboneIO().flip()

  def IO() = io
  def inAddressSpace(address: UInt) = address === UInt(i)
}

object nMasters { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleMaster( )) }
object nSlaves  { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleSlave (i)) }

class WishboneSharedBusInterconnectionSpec extends ChiselPropSpec {
  // assertTesterFails is not yet in a released version of Chisel3. But
  // it is coming ...
  // TODO: Remove this function after the next Chisel3 release.
  def assertTesterFails(t: => BasicTester, additionalVResources: Seq[String] = Seq()): Unit = {
    assert(!runTester(t, additionalVResources))
  }


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

  property("In a 1x1 bus, the slave receives a strobe when the master makes a request"){
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

  property("In a 1x1 bus, when the master isn't strobing the slave won't receive a strobe."){
    assertTesterPasses{
      new BasicTester{
        val max = 3
        val cnt = Counter(max)
        when( cnt.inc() ) { stop() }

        val slave = Module(new ExampleSlave(0))
        WishboneSharedBusInterconnection(
          Module(new Module with WishboneMaster { val io = new WishboneIO(); def IO() = io }),
          slave
        )
        Chisel.assert(slave.io.strobe === Bool(false))
      }
    }
  }

  property("Only one slave recieves a strobe at a time"){
    assertTesterPasses{
      new BasicTester{
        val slaves = nSlaves(2)
        val masters = nMasters(3)
        WishboneSharedBusInterconnection(
          masters,
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

  property("An assert will trigger if the slave address spaces overlap"){
    assertTesterFails{ // NB: assertTesterFails, not assertTesterPasses
      new BasicTester
      {
        val slave_0 = Module(new ExampleSlave(0))
        val slave_1 = Module(new ExampleSlave(0))
        val master  = Module(new ExampleMaster( ))

        WishboneSharedBusInterconnection(
          master,
          List(slave_0, slave_1)
        )

        val cnt = Counter(6); when( cnt.inc() ) { stop() };
      }
    }
  }

  property("""'Once the bus is arbitrated, the output signals from the
           selected MASTER are routed, via multiplexors, onto the
           shared buses. For example, if MASTER #0 obtains the bus,
           then the address lines [ADR_O()] from MASTER #0 are routed
           to shared bus [ADR()]. The same thing happens to the data
           out [DAT_O()], select out [SEL_O()], write enable [WE_O]
           and strobe [STB_O] signals. The shared bus output signals
           are routed to the inputs on the SLAVE interfaces.' --
           Wishbone B4"""){
    assertTesterPasses{
      new BasicTester
      {
        val masters = nMasters(2)
        val slaves = nSlaves(2)
        WishboneSharedBusInterconnection(
          masters,
          slaves
        )

        val master = masters(0)
        for (slave <- slaves) {
          Chisel.assert(slave.io.address     === master.io.address)
          Chisel.assert(slave.io.dataToSlave === master.io.dataToSlave)
          Chisel.assert(slave.io.select.asUInt === master.io.select.asUInt)
        }
        stop()
      }
    }
  }
}
