{- |
Module      : Benchmark.Plotting
Description : KDE distribution plots
Stability   : experimental

Generates kernel density estimate plots comparing primary and candidate
latency distributions with percentile markers.
-}
module Benchmark.Plotting (plotDistributions) where

import Data.Colour (withOpacity)
import Data.Colour.Names
import Data.Vector.Unboxed qualified as V
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Graphics.Rendering.Chart.Easy
import Statistics.Quantile (cadpw, quantile)
import Statistics.Sample (mean, stdDev)

-- | Silverman's rule of thumb for KDE bandwidth.
bandwidth :: V.Vector Double -> Double
bandwidth samples =
    let n = fromIntegral $ V.length samples
        sd = stdDev samples
     in 0.9 * sd * (n ** (-0.2))

gaussianKernel :: Double -> Double -> Double -> Double
gaussianKernel h x xi =
    let z = (x - xi) / h
     in exp (-(0.5 * z * z)) / (h * sqrt (2 * pi))

-- | Compute kernel density estimate at n points.
kde :: V.Vector Double -> Int -> [(Double, Double)]
kde samples point =
    let h = bandwidth samples
        samplesList = V.toList samples
        xmin = V.minimum samples - 3 * stdDev samples
        xmax = V.maximum samples + 3 * stdDev samples
        step = (xmax - xmin) / fromIntegral point
        xs = [xmin, xmin + step .. xmax]
        n = fromIntegral $ length xs
        density x = sum [gaussianKernel h x xi | xi <- samplesList] / n
     in [(x, density x) | x <- xs]

percentiles :: V.Vector Double -> [(String, Double)]
percentiles samples =
    [ ("p50", quantile cadpw 2 4 samples)
    , ("p95", quantile cadpw 19 20 samples)
    , ("p99", quantile cadpw 99 100 samples)
    ]

-- | Generate KDE plot comparing two distributions.
plotDistributions :: [Double] -> [Double] -> FilePath -> IO ()
plotDistributions primary candidate outFile = do
    let primaryVec = V.fromList primary
    let candidateVec = V.fromList candidate
    let primaryKDE = kde primaryVec 200
    let candidateKDE = kde candidateVec 200
    let primaryPercentiles = percentiles primaryVec
    let candidatePercentiles = percentiles candidateVec
    let maxDensity = maximum $ map snd (primaryKDE ++ candidateKDE)

    toFile def outFile $ do
        layout_title .= "Performance Distribution Comparison"
        layout_x_axis . laxis_title .= "Time (ms)"
        layout_y_axis . laxis_title .= "Density"

        plot $ line "Primary" [primaryKDE]
        plot $ line "Candidate" [candidateKDE]

        plot $ liftEC $ do
            plot_lines_style . line_color .= opaque blue
            plot_lines_style . line_dashes .= [5, 5]
            plot_lines_values
                .= [[(p, 0), (p, maxDensity)] | (_, p) <- primaryPercentiles]

        plot $ liftEC $ do
            plot_lines_style . line_color .= opaque red
            plot_lines_style . line_dashes .= [5, 5]
            plot_lines_values
                .= [[(p, 0), (p, maxDensity)] | (_, p) <- candidatePercentiles]

        setColors [opaque blue, opaque red]
