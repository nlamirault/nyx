// Copyright (C) 2016, 2017  Nicolas Lamirault <nicolas.lamirault@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

//     http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package render

import (
	"bytes"

	"github.com/BurntSushi/freetype-go/freetype/truetype"
	"github.com/BurntSushi/xgbutil/xgraphics"
	"github.com/martine/gocairo/cairo"
)

func Extents(text string, font *truetype.Font, size float64) (int, int) {
	return xgraphics.Extents(font, size, text)
}

func Text(text, font string, color Color, size float64, width, height int) ([]byte, error) {
	surf := cairo.ImageSurfaceCreate(cairo.FormatARGB32, width, height)

	cr := cairo.Create(surf.Surface)

	fr, fg, fb := color.ToFloat64s()
	cr.SetSourceRGB(0.2, 0.2, 0.2)
	cr.PaintWithAlpha(1.0)
	cr.SetAntialias(cairo.AntialiasBest)
	cr.SetSourceRGB(fr, fg, fb)
	cr.SelectFontFace(font, cairo.FontSlantNormal, cairo.FontWeightNormal)
	cr.SetFontSize(size)
	cr.MoveTo(float64(width)/10, float64(height)/2)
	cr.ShowText(text)

	buf := bytes.NewBuffer(nil)
	if err := surf.WriteToPNG(buf); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}
