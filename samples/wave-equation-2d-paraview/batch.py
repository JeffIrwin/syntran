
import argparse
import sys

from paraview.simple import *

paraview.simple._DisableFirstRenderCameraReset()

def exportPng(f, png):

    print("Reading \"" + f + "\" ...")
    sys.stdout.flush()

    displayRoot = LegacyVTKReader(FileNames=[f])

    renderView = GetActiveViewOrCreate("RenderView")
    renderView.InteractionMode = '3D'

    warp = WarpByVector(Input=displayRoot)
    warp.Vectors = ['POINTS', 'Amplitude']
    warp.ScaleFactor = 10.0

    #display = Show(displayRoot, renderView)
    display = Show(warp, renderView)
    #display = Show(displayRoot, warp)

    #display.SetRepresentationType('Surface With Edges')
    display.SetRepresentationType('Surface')
    #display.Opacity = 0.666
    #display.LineWidth = 4.0

    LUT = GetColorTransferFunction('Amplitude')
    PWF = GetOpacityTransferFunction('Amplitude')

    display.Representation = 'Surface'
    display.ColorArrayName = ['POINTS', 'Amplitude']
    display.LookupTable = LUT
    display.OSPRayScaleArray = 'Amplitude'
    display.OSPRayScaleFunction = 'PiecewiseFunction'
    display.SelectOrientationVectors = 'Amplitude'
    display.ScaleFactor = 10.0
    display.SelectScaleArray = 'Amplitude'
    display.GlyphType = 'Arrow'
    display.GlyphTableIndexArray = 'Amplitude'
    display.GaussianRadius = 0.5
    display.SetScaleArray = ['POINTS', 'Amplitude']
    display.ScaleTransferFunction = 'PiecewiseFunction'
    display.OpacityArray = ['POINTS', 'Amplitude']
    display.OpacityTransferFunction = 'PiecewiseFunction'
    display.DataAxesGrid = 'GridAxesRepresentation'
    display.PolarAxes = 'PolarAxesRepresentation'
    display.ScalarOpacityFunction = PWF
    display.ScalarOpacityUnitDistance = 6.570443907947594

    ColorBy(display, ('POINTS', 'Amplitude', 'Z'))
    LUT.RescaleTransferFunction(-1.0, 1.0)
    PWF.RescaleTransferFunction(-1.0, 1.0)
    LUT.ApplyPreset('Blue to Red Rainbow', True)

    renderView.ResetCamera()

    (min0,max0,min1,max1,min2,max2) = displayRoot.GetDataInformation().GetBounds()
    cen0 = 0.5 * (min0 + max0)
    cen1 = 0.5 * (min1 + max1)
    cen2 = 0.5 * (min2 + max2)

    #print(f"cen0 = {cen0}")
    #print(f"cen1 = {cen1}")
    #print(f"cen2 = {cen2}")

    renderView.CameraViewUp = [0, 0, 1]
    renderView.CameraPosition = [cen0 - 1, cen1 - 2, cen2 + 3]
    #renderView.CameraPosition = [49, 48, 3]
    renderView.CameraFocalPoint = [cen0, cen1, cen2]

    ## This is slow, but the edges look better (even better than just increasing
    ## the line width)
    #renderView.EnableRayTracing = 1

    renderView.ResetCamera()
    renderView.Update()

    #SaveScreenshot(png, renderView, ImageResolution=[540,540])
    #SaveScreenshot(png, renderView, ImageResolution=[1080,1080])
    SaveScreenshot(png, renderView, ImageResolution=[1920, 1080])
    #SaveScreenshot(png, renderView, ImageResolution=[2160,2160])
    #SaveScreenshot(png, renderView, ImageResolution=[3840,3840])

    Delete(renderView)
    del renderView

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--files",
        help = "List of data files to read",
        nargs = "+",
        type = str,
        required = True)
args = parser.parse_args()

for myfile in args.files:
    png = myfile.replace(".vtk", "") + ".png"
    exportPng(myfile, png)

