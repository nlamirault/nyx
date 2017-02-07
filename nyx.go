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

package main

import (
	"flag"
	"fmt"
	"os"
	"path"
	"runtime"
	"strings"

	"github.com/golang/glog"
	"github.com/mikkeloscar/go-wlc"

	"github.com/nlamirault/nyx/compositor"
	"github.com/nlamirault/nyx/version"
)

const (
	application = "Nyx"
)

var (
	flagDebug   bool
	flagVersion bool
	flagReplace bool
)

func init() {
	// parse flags
	flag.BoolVar(&flagVersion, "version", false, "show version")
	flag.Usage = usage
	flag.Parse()
}

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: %s [flags]\n", path.Base(os.Args[0]))
	flag.VisitAll(func(fg *flag.Flag) {
		fmt.Printf("--%s=\"%s\"\n\t%s\n", fg.Name, fg.DefValue,
			strings.Replace(fg.Usage, "\n", "\n\t", -1))
	})
	os.Exit(1)
}

func log(typ wlc.LogType, str string) {
	glog.V(2).Infof("Log: %d: %s\n", typ, str)
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	if flagVersion {
		fmt.Printf("%s v%s\n", application, version.Version)
		os.Exit(0)
	}

	glog.Infof("Nyx starting")

	wlc.LogSetHandler(log)
	if !wlc.Init() {
		os.Exit(1)
	}

	// Create the Wayland compositor
	compositor, err := compositor.New()
	if err != nil {
		glog.Fatalf("Can't create compositor: %s", err)
	}

	wlc.SetOutputResolutionCb(compositor.OutputResolution)

	wlc.SetViewCreatedCb(compositor.ViewCreated)
	wlc.SetViewDestroyedCb(compositor.ViewDestroyed)
	wlc.SetViewFocusCb(compositor.ViewFocus)
	wlc.SetViewRequestMoveCb(compositor.ViewRequestMove)
	wlc.SetViewRequestResizeCb(compositor.ViewRequestResize)

	wlc.SetKeyboardKeyCb(compositor.KeyboardKey)

	wlc.SetPointerButtonCb(compositor.PointerButton)
	wlc.SetPointerMotionCb(compositor.PointerMotion)

	wlc.Run()
	os.Exit(0)
}
