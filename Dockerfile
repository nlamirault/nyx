# Copyright (C) 2016, 2017 Nicolas Lamirault <nicolas.lamirault@gmail.com>

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

FROM golang:1.8.3-alpine
MAINTAINER Nicolas Lamirault <nicolas.lamirault@gmail.com>

COPY . /go/src/github.com/nlamirault/nyx

RUN set -x \
    && cd /go/src/github.com/nlamirault/nyx \
    && go build -o /usr/bin/nyx . \
    && echo "Build complete."

ENTRYPOINT [ "/usr/bin/nyx" ]
