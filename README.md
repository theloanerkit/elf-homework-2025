# What?
This repo contains my (attempted) solutions for [Advent of Code](https://adventofcode.com/) 2025, a yearly collection of coding challenges written by Eric Wastl.

# Which langauge?
There are two langauges that I have experience with: [Python](https://www.python.org/) and [Fortran](https://fortran-lang.org/). Python is a very widely used, incredibly flexible language with all sorts of libraries and packages to solve almost any problem. Fortran is a high-performance language used by people who need to do difficult maths *fast*. The natural choice for a language to use for Advent of Code would be Python, so I'm doing it in Fortran.

OK. I'm not doing it *entirely* in Fortran. I've got a Python parser to do an initial sweep on the inputs to make them slightly more friendly, and a bash script or two to make running things easier.

# Why?
I enjoy coding (strange, I know), and this gives me a place to put my code so that I can find it again in 6 months time.
### Why *Fortran*?
A few reasons

1. I like a challenge

I have done Advent of Code in Python in the past, and thoroughly enjoyed the experience, but I like making life difficult for myself.

2. It's a good way to learn a language

This one doesn't apply as much as it did the first time I did AoC in Fortran, but I'm still learning new things about the language.

3. Potential parallelism

One of the things that is much easier in Fortran than Python is parallelism via OpenMP, MPI, or both. 

# How's it going?
### 3pm, December 5, 2025
I haven't started yet. I started my PhD a few months ago and it has been *busy*. I have just set up my repo, so hopefully I'll get to start in the next few days.
### 5:45pm, December 6, 2025
Officially started! Day 1, part 1 done. Started setting up the helper functions, so a bit slow to begin with.
### 11:05am, December 7, 2025
Day 1, part 2 done. That took too long. The perils of hoping half-assing it will work out. I started by trying to just extend my part 1 solution with a few bodges and an attempt to fix double counting. Evidently this did not work. Spent several hours going further into the spiral of trying to make modular arithmetic and integer division work before giving up and rewriting the entire thing.
### 8:20am, December 8, 2025
Day 2, part 1 done. I started this on the train yesterday and spend *a while* breaking my string conversion functions. Such things do happen when you do AoC in a language like Fortran. Otherwise, I really enjoyed this puzzle. I considered brute-forcing it and checking every number, but in the end I settled on iterating through *half* of the number and concatenating the two together to generate the numbers to check. I did spend a while using $\left\lceil\log_{10}(x)\right\rceil$ when I should have been using $\left\lfloor\log_{10}(x)\right\rfloor+1$ though.