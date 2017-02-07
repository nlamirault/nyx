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

package keyboard

import (
	//"fmt"

	"github.com/mikkeloscar/go-xkbcommon"
)

type Keyboard struct {
	keySyms  map[xkb.KeySym]struct{}
	keyCodes map[uint32]struct{}
}

// New initializes a new keyboard.
func New() *Keyboard {
	return &Keyboard{
		keySyms:  make(map[xkb.KeySym]struct{}),
		keyCodes: make(map[uint32]struct{}),
	}
}

// PressKey adds a key to the keystate.
func (k *Keyboard) PressKey(keySym xkb.KeySym, keyCode uint32) {
	if keyCode == 0 {
		return
	}

	if !k.IsPressed(keySym, keyCode) {
		k.keyCodes[keyCode] = struct{}{}
		k.keySyms[keySym] = struct{}{}
	}

}

// ReleaseKey removes a key from the keystate.
func (k *Keyboard) ReleaseKey(keySym xkb.KeySym, keyCode uint32) {
	if k.IsPressed(keySym, keyCode) {
		delete(k.keySyms, keySym)
		delete(k.keyCodes, keyCode)
	}
}

// IsPressed returns true if key is pressed.
func (k *Keyboard) IsPressed(keySym xkb.KeySym, keyCode uint32) bool {
	if _, ok := k.keySyms[keySym]; ok {
		return true
	}

	if _, ok := k.keyCodes[keyCode]; ok {
		return true
	}

	return false
}
