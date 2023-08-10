package pseudocode_fft

import pseudocode_fft.CooleyTurkey.{fft, ifft}


object CooleyTurkey_Tests extends FFT_TestTemplate(fft,ifft)
