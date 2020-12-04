Balanced sliding window is a protocol used where reliable in-order delivery
of packets is required (like TCP).

**Course**: [Software Foundations], Spring 2020<br>
**Taught by**: Prof. Venkatesh Chopella

[Balanced sliding window], (or Selective Repeat sliding window) is modeled as
transition system following in stages:
1. Manual send
2. Automatic send (Next)
3. Automatic send (Next, Next 100)

Two communicating processes **P** and **Q** send packets to each other,
through a channel. **P** is considered to be the main sending process, and
**Q** is sending acknowledgements. Each process has a send and a recieve
buffer, and the channel is thought of as having 2 buffers (P->Q, Q->P).

See [Report.pdf] for theory and proof, and [Implementation.pdf] for examples.

[Software Foundations]: https://github.com/iiithf/software-foundations
[Balanced sliding window]: https://cse.iitkgp.ac.in/~pallab/dist_sys/Lec-02-SlidingWindow.pdf
[Report.pdf]: docs/Report.pdf
[Implementation.pdf]: docs/Implementation.pdf
