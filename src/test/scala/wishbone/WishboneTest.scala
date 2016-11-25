// See LICENSE for license details.

package wishbone.test

import org.scalatest._
import org.scalatest.prop._
import Chisel.iotesters._
import Chisel.testers._
import wishbone._

import chisel3._
import chisel3.util._
import chisel3.core.SeqUtils
import chisel3.iotesters._

class ExampleMaster extends Module with WishboneMaster {
  val io = IO(new WishboneIO())

  io.cycle   := Bool(true)
  io.strobe  := Bool(true)
  io.address := 0.U

  io.dataToSlave := UInt(1)

  val myVec = Wire( Vec(32 / 8, Bool()) )
  myVec foreach (_ := Bool(true))

  io.select      := myVec
  io.writeEnable := Bool(false)
}

class ExampleSlave(i: Int) extends Module with WishboneSlave {
  val io = IO(Flipped(new WishboneIO()))

  def inAddressSpace(address: UInt) = address === UInt(i)

  io.ack := io.cycle && io.strobe
}

object nMasters { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleMaster( )) }
object nSlaves  { def apply(i: Int) = for (i <- 0 until i) yield Module(new ExampleSlave (i)) }

abstract class WishbonePropSpec extends fixture.PropSpec with ChiselRunners with PropertyChecks {
  // assertTesterFails is not yet in a released version of Chisel3. But
  // it is coming ...
  // TODO: Remove this function after the next Chisel3 release.
  def assertTesterFails(t: => BasicTester, additionalVResources: Seq[String] = Seq()): Unit = {
    assert(!runTester(t, additionalVResources))
  }

  type FixtureParam = BusType

  def getBus: BusType

  override def withFixture(test: OneArgTest) = { test(getBus) }
}

class SharedBusSpec extends CommonSpec { def getBus = SharedBus }
class CrossbarSpec  extends CommonSpec { def getBus = Crossbar  }

abstract class CommonSpec extends WishbonePropSpec {
  property("Compiles with x masters and y slaves, for x,y in [0,3]"){
      bus: BusType => assertTesterPasses{
        new BasicTester {
          for(num_masters <- 0 to 3; num_slaves <- 0 to 3) {
            WishboneInterconnection(bus,
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
    bus: BusType => assertTesterPasses{
      new BasicTester {
        val mastersIO = nMasters(1)
        val slavesIO  = nSlaves (1)

        WishboneInterconnection(bus,
          mastersIO,
          slavesIO
        )
        Chisel.assert(slavesIO(0).io.strobe)
        stop()
      }
    }
  }

  property("In a 1x1 bus, when the master isn't strobing the slave won't receive a strobe."){
    bus: BusType => assertTesterPasses{
      new BasicTester{
        val max = 3
        val cnt = Counter(max)
        when( cnt.inc() ) { stop() }

        val slave = Module(new ExampleSlave(0))
        WishboneInterconnection(bus,
          Module(new Module with WishboneMaster { val io = IO(new WishboneIO()); }),
          slave
        )
        Chisel.assert(slave.io.strobe === Bool(false))
      }
    }
  }

  property("Only one slave recieves a strobe at a time"){
    bus: BusType => assertTesterPasses{
      new BasicTester{
        val slaves = nSlaves(2)
        val masters = nMasters(3)
        WishboneInterconnection(bus,
          masters,
          slaves
        )
        Chisel.assert(slaves(0).io.strobe ^ slaves(1).io.strobe)
        stop()
      }
    }
  }

  property("A slave only receives accesses for it's address range"){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val slave = Module(
          new Module with WishboneSlave {
            val io = IO(Flipped(new WishboneIO()))
            def inAddressSpace(address: UInt) : Bool = address === UInt(4)
          }
        )

        val master = Module( new Module with WishboneMaster {
          val io = IO(new WishboneIO())
          io.cycle := Bool(true)
          io.strobe := Bool(true)
          val (cnt, done) = Counter(!reset, 20)
          io.address := cnt
        })

        WishboneInterconnection(bus,
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
    bus: BusType => assertTesterFails{ // NB: assertTesterFails, not assertTesterPasses
      new BasicTester
      {
        val slave_0 = Module(new ExampleSlave(0))
        val slave_1 = Module(new ExampleSlave(0))
        val master  = Module(new ExampleMaster( ))

        WishboneInterconnection(bus,
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
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val masters = nMasters(2)
        val slaves = nSlaves(2)
        WishboneInterconnection(bus,
          masters,
          slaves
        )

        val master = masters(0)
        for (slave <- slaves) {
          Chisel.assert(slave.io.address       === master.io.address)
          Chisel.assert(slave.io.dataToSlave   === master.io.dataToSlave)
          Chisel.assert(slave.io.select.asUInt === master.io.select.asUInt)
        }
        stop()
      }
    }
  }
  property("""'[the terminating signals [ACK_I], [RTY_I] and [ERR_I]]
           are enabled at the MASTER that acquired the bus. For
           example, if MASTER #0 is granted the bus by the arbiter,
           then the [ACK_I], [RTY_I] and [ERR_I] are enabled at MASTER
           #0.' -- Wishbone B4"""){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val masters = nMasters(2)
        val slaves = nSlaves(2)
        WishboneInterconnection(bus,
          masters,
          slaves
        )

        val master = masters(0)

        val (cnt, done) = Counter(!reset, 20)

        when(cnt === 0.U) { Chisel.assert(master.io.ack) }

        when(done) { stop() }
      }
    }
  }
  property("""'[the terminating signals [ACK_I], [RTY_I] and [ERR_I]]
           are not enabled at the masters that don't aquire the bus.'
           -- Wishbone B4, page 120 """){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val masters = nMasters(2)
        val slaves = nSlaves(2)
        WishboneInterconnection(bus,
          masters,
          slaves
        )

        val disabled_master = masters(1)

        val (cnt, done) = Counter(!reset, 20)

        when(cnt === 0.U) { Chisel.assert(disabled_master.io.ack === Bool(false)) }

        when(done) { stop() }
      }
    }
  }
  property("""An assertion is triggered if standard mode SLAVE
           interfaces don't negate [ACK_O] when their [STB_I] is
           negated.
           -- B4, OBSERVATION 3.10"""){
    bus: BusType => assertTesterFails{
      new BasicTester
      {
        WishboneInterconnection(bus,
          // A master that negates strobe
          Module(
            new Module with WishboneMaster {
              val io = IO(new WishboneIO())
              io.cycle := Bool(true)
              io.strobe := Bool(false)
              io.address := 0.U
            }
          ),
          // A slave that violates the assertion.
          Module(
            new Module with WishboneSlave {
              val io = IO(Flipped(new WishboneIO()))
              def inAddressSpace(address: UInt) = Bool(true)

              // Slave drives ack, without checking if strobe is high.
              io.ack := Bool(true)
            }
          )
        )

        stop()
      }
    }
  }
  property("""Asynchronous slaves are supported."""){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val master = Module(
          new Module with WishboneMaster {
            val io = IO(new WishboneIO())
            io.cycle   := Counter(!reset, 2)._2
            io.strobe  := Bool(true)
            io.address := 0.U
          }
        )

        val slave = Module(
          new Module with WishboneSlave {
            val io = IO(Flipped(new WishboneIO()))
            def inAddressSpace(address: UInt) = Bool(true)
            io.ack := io.cycle && io.strobe
          }
        )

        WishboneInterconnection(bus,
          master,
          slave
        )

        Chisel.assert(master.io.ack === master.io.cycle)

        val cnt = Counter(6); when( cnt.inc() ) { stop() };
      }
    }
  }
  property("""Slow synchronous masters will be granted the bus until they are done with it."""){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        // Create 2 masters
        val masters = for (i <- 1 to 2) yield Module(
          new Module with WishboneMaster {
            val io = IO(new WishboneIO())
            // Drive the cycle signal for several cycles, start
            // driving it after 1 and 2 cycles respectively for the
            // two masters.
            io.cycle   := Counter(!reset, 6)._1 >= i.U
            // Have the strobe signal lagging 1 cycle behind.
            io.strobe  := RegNext(io.cycle)
            io.address := Mux(io.cycle && io.strobe, i.U, 0.U)
          }
        )

        val slave = Module(
          new Module with WishboneSlave {
            val io = IO(Flipped(new WishboneIO()))
            def inAddressSpace(address: UInt) = Bool(true)
            io.ack := io.cycle && io.strobe
          }
        )

        WishboneInterconnection(bus,
          masters,
          slave
        )

        val master = masters(0)
        // masters(0) should get the bus because he requests it with
        // his cycle signal one cycle before the other master.
        val cnt = Counter(!reset, 16)
        switch(cnt._1){
          is(0.U) {
            Chisel.assert(slave.io.strobe === 0.U)
          }
          is(1.U) {
            Chisel.assert(slave.io.strobe === 0.U)
          }
          is(2.U) {
            Chisel.assert(slave.io.cycle   === 1.U)
            Chisel.assert(slave.io.strobe  === 1.U)
            Chisel.assert(slave.io.address === 1.U)
          }
          is(3.U) {
            Chisel.assert(slave.io.cycle   === 1.U)
            Chisel.assert(slave.io.strobe  === 1.U)
            Chisel.assert(slave.io.address === 1.U)
            Chisel.assert(slave.io.ack)
            Chisel.assert(master.io.ack)
          }
          is(4.U) {
            Chisel.assert(slave.io.cycle   === 1.U)
            Chisel.assert(slave.io.strobe  === 1.U)
            Chisel.assert(slave.io.address === 1.U)
            Chisel.assert(slave.io.ack)
            Chisel.assert(master.io.ack)
          }
          is(5.U) {
            stop()
          }
        }

      }
    }
  }
  property("""A master will hold the bus until it is released."""){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val masters = nMasters(2)
        WishboneInterconnection(bus,
          masters,
          nSlaves(1)
        )
        val cnt = Counter(!reset, 6)

        for ( master <- masters)
          when(cnt._1 > 1.U) { Chisel.assert(master.io.ack === RegNext(master.io.ack)) }

        when( cnt._2 ) { stop() }
      }
    }
  }
  property("""When only one master is requesting the bus there is no
           delay before the bus is granted to it."""){
    bus: BusType => assertTesterPasses{
      new BasicTester
      {
        val a_bus_requesting_master = Module(
          new Module with WishboneMaster {
            val io = IO(new WishboneIO())
            io.cycle   := !reset
          }
        )

        val non_bus_requesting_masters = for( i <- 1 to 4) yield Module(
          new Module with WishboneMaster {
            val io = IO(new WishboneIO())
            io.cycle   := Bool(false)
          }
        )

        val slaves = nSlaves(6)
        val masters = non_bus_requesting_masters ++ List(a_bus_requesting_master)

        WishboneInterconnection(bus,
          masters,
          slaves
        )

        when(!reset) {
          val bus_has_been_granted = slaves(0).io.cycle
          Chisel.assert(bus_has_been_granted)
          stop()
        }
      }
    }
  }
}
