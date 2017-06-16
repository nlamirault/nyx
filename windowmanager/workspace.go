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

package windowmanager

import (
	// "fmt"

	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/xwindow"

	"github.com/nlamirault/nyx/config"
)

type Workspace struct {
	name   string
	window *xwindow.Window
}

func newWorkspace(x *xgbutil.XUtil, conf *config.Workspace) (*Workspace, error) {
	x.Grab()
	defer x.Ungrab()

	rg := xwindow.RootGeometry(x)
	ws, err := xwindow.Generate(x)
	if err != nil {
		return nil, err
	}
	if err := ws.CreateChecked(x.RootWin(), 0, 20, rg.Width(), rg.Height()-20, xproto.CwBackPixel, 0x666666); err != nil {
		return nil, err
	}
	if err := xproto.ReparentWindowChecked(x.Conn(), ws.Id, x.RootWin(), 0, 20).Check(); err != nil {
		return nil, err
	}
	return &Workspace{
		window: ws,
		// name:   fmt.Sprintf("%s %s", conf.Icon, conf.Name),
		name: conf.Name,
	}, nil
}

func (ws *Workspace) Name() string {
	return ws.name
}

func (ws *Workspace) Deactivate() {
	ws.window.Unmap()
}

func (ws *Workspace) Activate() {
	ws.window.Map()
}

func (ws *Workspace) WindowId() xproto.Window {
	return ws.window.Id
}
