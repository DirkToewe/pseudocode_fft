package pseudocode_fft

import pseudocode_fft.CooleyTukey.{fft, ifft}


object CooleyTukey_Tests extends FFT_TestTemplate(fft,ifft)
