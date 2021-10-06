module Icons exposing (club, diamond, heart, spade)

import Svg exposing (Svg)
import Svg.Attributes


club : String -> String -> Svg msg
club color opacity =
    Svg.path
        [ Svg.Attributes.fill color
        , Svg.Attributes.opacity opacity
        , Svg.Attributes.d "M30.14,41.83c.64,0,1.28,0,1.91.06a30.14,30.14,0,1,1,55.52,0c.61,0,1.21-.06,1.83-.06A30.14,30.14,0,1,1,65.55,90.38c5.35,10,11.91,20.24,22.73,25.61v6.89h-57V116c8.36-3,15-12.62,20.84-23.38a30.14,30.14,0,1,1-22-50.78Z"
        ]
        []


diamond : String -> String -> Svg msg
diamond color opacity =
    Svg.path
        [ Svg.Attributes.fill color
        , Svg.Attributes.opacity opacity
        , Svg.Attributes.d "M0,61.51A234.72,234.72,0,0,0,61.51,0a204.09,204.09,0,0,0,61.37,61.37,229.27,229.27,0,0,0-61.39,61.51A215.26,215.26,0,0,0,0,61.51Z"
        ]
        []


heart : String -> String -> Svg msg
heart color opacity =
    Svg.path
        [ Svg.Attributes.fill color
        , Svg.Attributes.opacity opacity
        , Svg.Attributes.d "M61.44,19.23A41.21,41.21,0,0,1,71.25,7.29C90.05-8,117.34,1.91,122.25,26A28.34,28.34,0,0,1,122,39.11c-2.28,8.31-9.89,16.72-18,24.42-3.89,3.69-7.64,7.13-11.32,10.56-11.45,10.73-22.72,22-31.18,35.28C53,96.07,41.71,84.82,30.25,74.09c-3.67-3.43-7.42-6.87-11.31-10.56-8.15-7.7-15.75-16.11-18-24.42A28.34,28.34,0,0,1,.63,26c4.91-24.11,32.2-34,51-18.73a41.24,41.24,0,0,1,9.82,11.94Z"
        ]
        []


spade : String -> String -> Svg msg
spade color opacity =
    Svg.path
        [ Svg.Attributes.fill color
        , Svg.Attributes.opacity opacity
        , Svg.Attributes.d "M62.61,91.7a35.65,35.65,0,0,0,5.2,5.22c17.88,14.5,43.85,5.1,48.52-17.79a26.74,26.74,0,0,0-.27-12.43c-2.17-7.89-9.4-15.88-17.15-23.19-3.71-3.5-7.27-6.76-10.77-10C77.7,23.73,66.06,12.12,58.47,0h0C50.87,12.12,39.23,23.73,28.79,33.48c-3.5,3.27-7.06,6.53-10.77,10C10.27,50.82,3,58.81.87,66.7A26.74,26.74,0,0,0,.6,79.13C5.27,102,31.24,111.42,49.12,96.92A35.68,35.68,0,0,0,54.88,91c-5.41,12.62-13,21.27-23.79,25.27v6.62h55v-6.62C75.47,112.33,68,103.93,62.61,91.7Z"
        ]
        []
