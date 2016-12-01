Chisel Wishbone NoC generator
=======================

This project is WIP and is not ready to be used.

The goal of this project is to make it easy and safe for Chisel SoC's
to interconnect slaves and masters using the Wishbone protocol.

# TODO
- [x] Maintain a unit test suite for all supported features
- [x] Point-to-point interconnnections
  - [x] Trivially supported with a Bundle
- [x] Shared-bus interconnnections
  - [x] Discontinuous and individually-sized slave memory maps
    - [ ] Benchmark partial address decoding against current implementation
  - [ ] Verify slave protocol compliance at simulation time
    - [x] Assert that ¬STB_I → ¬ACK_O
  - [ ] Verify master protocol behaviour at simulation time
    - [ ] Assert if a master hogs the bus for x cycles
  - [ ] User-friendly support for normal segmented memory maps
  - [ ] Multiple arbitration strategies
- [x] Crossbar interconnnections
- [ ] Make the generated code more readable
