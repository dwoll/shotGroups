fluidPage(
    fluidRow(
        box(title="Background math",
            width=12,
            img(src="AnglesCircle.png", width=150),
            p("Angle \\(\\varphi\\) (in degree) with corresponding arc length
                  \\(x\\) (in radian) in the unit circle."),
            p("In addition to absolute length units, group size is often reported in terms
                      of its angular diameter. Angles can be measured equivalently either in degree
                      or in radian. If \\(x\\) is the angular measurement in radian, and \\(\\varphi\\)
                      the angular measurement in degree for the same angle, then
                      \\(\\frac{x}{2 \\pi} = \\frac{\\varphi}{360}\\) such that conversion between
                      degree and radian is given by \\(x = \\frac{2 \\pi}{360} \\cdot \\varphi\\) and
                      \\(\\varphi = \\frac{360}{2 \\pi} \\cdot x\\)."),
            p("The angular size of an object with absolute size \\(s\\)
                      is its angular diameter at a given distance \\(d\\). This
                      is the angle \\(\\alpha\\) subtended by the object with the
                      line of sight centered on it."),
            img(src="AnglesCenterH.jpg", width=300),
            p("Angular diameter of object with absolute size \\(s\\) at
                      distance to target \\(d\\). Right triangle formed by \\(d\\)
                      and object of size \\(s/2\\). \\(s\\) corresponds to angle
                      \\(\\alpha\\) (degree) and arc length \\(x\\) (radian)."),
            p("The following measures for angular size are supported:"),
            HTML("<ul>
                  <li>deg = degree. The circle is divided into 360 degrees.</li>
                  <li>MOA = minute of angle = arcmin. 1 MOA = \\(1/60\\) degree
                      such that the circle has \\(360 \\cdot 60 = 21600\\) MOA.</li>
                  <li>SMOA = Shooter's MOA = Inches Per Hundred Yards IPHY.
                      1 inch at 100 yards = 1 SMOA.</li>
                  <li>rad = radian. 1 radian is 1 unit of arc length on
                      the unit circle which has a circumference of \\(2 \\pi\\).
                      The circle circumference is divided into \\(2 \\pi\\) rad.</li>
                  <li>mrad = milliradian = 1/1000 radian. The circle circumference is
                      divided into \\(2 \\pi \\cdot 1000 \\approx 6283.19\\) mrad.</li>
                  <li>mil: 1 mil = \\(\\frac{2 \\pi}{6400}\\) radian -
                      the circle circumference is divided into 6400 mils.</li>
                  </ul>")
        )
    )
)
