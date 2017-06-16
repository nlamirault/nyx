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

type Color struct {
	R uint32
	G uint32
	B uint32
	A uint32
}

func NewColor(r, g, b uint32) Color {
	return Color{r, g, b, 255}
}

func (c Color) ToFloat64s() (r, g, b float64) {
	r = float64(c.R) / 255.0
	g = float64(c.G) / 255.0
	b = float64(c.B) / 255.0

	return
}

func (c Color) RGBA() (r, g, b, a uint32) {
	return c.R, c.G, c.B, c.A
}
