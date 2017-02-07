// Copyright (C) 2016, 2017 Nicolas Lamirault <nicolas.lamirault@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

//     http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package layout

import (
	//"fmt"
	// "math"

	"github.com/golang/glog"
	"github.com/mikkeloscar/go-wlc"
)

// Vertical define the vertical split layout
type Vertical struct {
}

func (v *Vertical) Relayout(output wlc.Output) {
	glog.V(2).Info("Calculate layout")
	r := output.GetResolution()
	if r == nil {
		return
	}
	views := output.GetViews()
	glog.V(2).Infof("Views: %s Resolution: %d %d", views, r.W, r.H)
	toggle := false
	// y := 0
	// w := r.W / 2
	// h := r.H / uint32(math.Max(float64((1+len(views))/2), 1))
	y := 0
	w := r.W
	h := r.H
	if len(views) > 0 {
		w = r.W / uint32(len(views))
		h = r.H
	}
	glog.V(2).Infof("Views: %s %d %d", views, r.W, r.H)
	for i, view := range views {
		glog.V(2).Infof("View: %s Size: %d %d", view, w, h)
		geometry := wlc.Geometry{
			Origin: wlc.Point{
				X: 0,
				Y: int32(y),
			},
			Size: wlc.Size{
				W: w,
				H: h,
			},
		}

		if toggle {
			geometry.Origin.X = int32(w)
		}

		if !toggle && i == len(views)-1 {
			geometry.Size.W = r.W
		}

		glog.V(2).Infof("Geometry view: %s %v", view, geometry)
		view.SetGeometry(0, geometry)

		toggle = !toggle
		if !toggle {
			y += int(h)
		}
	}
}
