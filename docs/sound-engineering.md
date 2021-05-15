# Digital Sound Engineering

## Sound

* With sound Sound, we mean vibrations that fall in the frequency range that
  humans can hear.

* When the membrane of a kick drum is hit, it starts to oscillate and so does
  the air around it.

* Then the air around that air starts to oscillate as well, and the vibration
  is seen as a acoustic wave, traveling through time and space.

* When the vibration (compression and decompression of air) hits our ear drum,
  and provided the air oscillates in the hearable range (~20Hz - ~20kHz), our
  brain perceives this as sound.

* Fixing the space origin to our ear, we can express sound as a two dimensional
  plot where the Y-axis represents air compression and the X-axis represents
  time.

## Digital

* With digital, we mean handling information with discrete values.

* The information we want to handle is the sound plot, but it is not discrete,
  it is continuous.

* We can however approximate the plot by picking a number of points on the plot
  and writing them out in succession.

* Now, the information is discrete, and anyone reading the sequence of points
  can to some degree reconstruct the shape of the original plots by connecting
  the dots in the 2D-plane.

* If we make sure that intervals between the points on the X-axis, we can save
  ourselves the trouble of writing out the x-values completely and only write
  out a succession of Y-values.

* Now, our sequence only contains half the numbers, but with no loss of
  information, given that we also told the interpreter the length of the
  X-interval.

* Commonly, this interval is called sample rate - the number of
  points sampled during a specific time. A common sample rate is 48000 samples
  per second.

* To further standardize our digital sound format, we make sure that the
  Y-values fall in range [0, 1]. The interpreter of our digital wave is then free to
  multiply each value with a scalar, to scale up or down the wave, without
  changing the shape of the wave.

## Engineering

* Instead of drawing a wave per hand and manually sampling the points, we can
  generate the points directly.

* For this, we use the sine-wave function, because it describes periodic
  oscillation, which is needed to generate sound.

* This function of time is defined as

```
y(t) = A * sin(w * t)
```
where `A` is the amplitude, or maximum absolute Y-value of the wave peaks, and
`w` is the angular frequency, or changes in radians per second.

We generate our samples by first generating a discrete series of timestamps.
Let's generate one second worth of timestamps.

```
sampleRate :: Double
sampleRate = 48000

type TimeStamp = Double

timestamps :: [TimeStamp]
timestamps = [1.0..sampleRate]

```

The `type` keyword is a type alias. Here, we alias the primitive haskell type
Double, (probably) representing a 64 bit floating port number. Why not use
integers for discrete numbers? As we will see, it will not make a difference
for the intents and purposes of this post, and it will save us the (sometimes
verbose) converting between different number types.



