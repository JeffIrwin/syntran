
import argparse
import sys

from paraview.simple import *

paraview.simple._DisableFirstRenderCameraReset()

def exportPng(f, png):

    print("Reading \"" + f + "\" ...")
    sys.stdout.flush()

    # Colorbar min/max ranges
    cmin = -0.2
    cmax =  0.2

    warpFactor = 50.0

    displayRoot = LegacyVTKReader(FileNames=[f])

    renderView = GetActiveViewOrCreate("RenderView")
    renderView.InteractionMode = '3D'

    vectorName = 'Amplitude'

    warp = WarpByVector(Input=displayRoot)
    warp.Vectors = ['POINTS', vectorName]
    warp.ScaleFactor = warpFactor

    #display = Show(displayRoot, renderView)
    display = Show(warp, renderView)
    #display = Show(displayRoot, warp)

    #display.SetRepresentationType('Surface With Edges')
    display.SetRepresentationType('Surface')
    #display.Opacity = 0.666
    #display.LineWidth = 4.0

    LUT = GetColorTransferFunction(vectorName)
    PWF = GetOpacityTransferFunction(vectorName)

    display.Representation = 'Surface'
    display.ColorArrayName = ['POINTS', vectorName]
    display.LookupTable = LUT
    #display.OSPRayScaleArray = vectorName
    #display.OSPRayScaleFunction = 'PiecewiseFunction'
    display.SelectOrientationVectors = vectorName
    display.ScaleFactor = warpFactor
    display.SelectScaleArray = vectorName
    #display.GlyphType = 'Arrow'
    #display.GlyphTableIndexArray = vectorName
    #display.GaussianRadius = 0.5
    display.SetScaleArray = ['POINTS', vectorName]
    display.ScaleTransferFunction = 'PiecewiseFunction'
    #display.OpacityArray = ['POINTS', vectorName]
    #display.OpacityTransferFunction = 'PiecewiseFunction'
    display.DataAxesGrid = 'GridAxesRepresentation'
    display.PolarAxes = 'PolarAxesRepresentation'
    #display.ScalarOpacityFunction = PWF
    #display.ScalarOpacityUnitDistance = 6.570443907947594

    ColorBy(display, ('POINTS', vectorName, 'Z'))
    LUT.RescaleTransferFunction(cmin, cmax)
    PWF.RescaleTransferFunction(cmin, cmax)

    #LUT.ApplyPreset('Blue to Red Rainbow', True)
    LUT.ApplyPreset('Plasma (matplotlib)', True)

    display.SetScalarBarVisibility(renderView, True)
    LUTColorBar = GetScalarBar(LUT, renderView)
    LUTColorBar.TitleFontSize = 4
    LUTColorBar.LabelFontSize = 4

    #renderView.ResetCamera()

    (min0,max0,min1,max1,min2,max2) = displayRoot.GetDataInformation().GetBounds()
    cen0 = 0.5 * (min0 + max0)
    cen1 = 0.5 * (min1 + max1)

    cen2 = 0.5 * (min2 + max2)
    #cent2 = 0.0

    #print(f"cen0 = {cen0}")
    #print(f"cen1 = {cen1}")
    #print(f"cen2 = {cen2}")

    cameraScale = 36
    cameraVec = [-1, -3, 6]
    cameraOffset = [
            cameraScale * cameraVec[0], 
            cameraScale * cameraVec[1],
            cameraScale * cameraVec[2]]

    renderView.CameraViewUp = [0, 0, 1]
    renderView.CameraPosition = [
            cen0 - cameraOffset[0], 
            cen1 - cameraOffset[1], 
            cen2 + cameraOffset[2] ]

    # Correct for perspective to center view
    renderView.CameraFocalPoint = [
            cen0 - 1.5 * cameraVec[0], 
            cen1 - 1.5 * cameraVec[1], 
            cen2 - 1.5 * cameraVec[2]]
    #renderView.CameraPosition = [49, 48, 3]

    ## This is slow, but the edges look better (even better than just increasing
    ## the line width)
    #renderView.EnableRayTracing = 1

    #renderView.ResetCamera()
    renderView.Update()

    #SaveScreenshot(png, renderView, ImageResolution=[540,540])
    #SaveScreenshot(png, renderView, ImageResolution=[1080,1080])
    #SaveScreenshot(png, renderView, ImageResolution=[1920, 1080])
    #SaveScreenshot(png, renderView, ImageResolution=[1920, 1080])
    #SaveScreenshot(png, renderView, ImageResolution=[2160,2160])
    #SaveScreenshot(png, renderView, ImageResolution=[3840,3840])
    SaveScreenshot(png, renderView, ImageResolution=[3840,2160])

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

