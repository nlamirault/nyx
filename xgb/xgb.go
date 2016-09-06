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

package xgb

import (
	"log"

	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/xevent"
)

type Xgb struct {
	X *xgbutil.XUtil
}

func New() (*Xgb, error) {
	log.Printf("[DEBUG] Create the Xgb wrapper")
	xgb := &Xgb{}
	if err := xgb.connect(); err != nil {
		return nil, err
	}
	if err := xgb.setupRoot(); err != nil {
		return nil, err
	}
	xgb.setupEvents()
	return xgb, nil
}

func (xgb *Xgb) connect() error {
	var err error
	if xgb.X, err = xgbutil.NewConn(); err != nil {
		return err
	}

	return nil
}

func (xgb *Xgb) EnterMain() {
	xevent.Main(xgb.X)
}

func (xgb *Xgb) Destroy() {
	xgb.X.Conn().Close()
}
