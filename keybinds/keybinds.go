// Copyright (C) 2016  Nicolas Lamirault <nicolas.lamirault@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

//     http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package keybinds

import (
	"log"

	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/keybind"
	"github.com/BurntSushi/xgbutil/xevent"
)

type Key struct {
	Mods uint16
	Code xproto.Keycode
	Cmd  string
}

type Keybinds struct {
	X    *xgbutil.XUtil
	Keys []*Key
}

// New creates a new Keybinds instance
func New(xu *xgbutil.XUtil, keys map[string]string) *Keybinds {
	keybind.Initialize(xu)
	kb := &Keybinds{
		X:    xu,
		Keys: make([]*Key, 0, len(keys)),
	}

	for keystr, cmd := range keys {
		mods, keycodes, err := keybind.ParseString(xu, keystr)
		if err != nil {
			log.Printf("[WARN] Error parsing keybind: %v", err)
			continue
		}
		key := &Key{
			Mods: mods,
			Code: keycodes[0],
			Cmd:  cmd,
		}
		log.Printf("[DEBUG] Add keybind : %v", key)
		kb.Keys = append(kb.Keys, key)

		if err := keybind.KeyPressFun(kb.keyPressFun).Connect(
			xu, xu.RootWin(), keystr, true); err != nil {
			log.Printf("[WARN] KeyPressFun error : %v", err)
		}
	}

	return kb
}
func (kb *Keybinds) keyPressFun(xu *xgbutil.XUtil, ev xevent.KeyPressEvent) {
	modStr := keybind.ModifierString(ev.State)
	keyStr := keybind.LookupString(xu, ev.State, ev.Detail)
	if len(modStr) > 0 {
		log.Printf("[DEBUG] Key: %s-%s\n", modStr, keyStr)
	} else {
		log.Println("[DEBUG] Key:", keyStr)
	}
	for _, key := range kb.Keys {
		if key.Code == ev.Detail {
			log.Printf("[DEBUG] Execute command: %s", key.Cmd)
			if key.Cmd == "exit" {
				xevent.Quit(xu)
			}
			return
		}
	}
}
