#|
Copyright (C) 2021  Anthony Green <green@redhat.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
|#

(asdf:defsystem #:tower-pull
  :description "Pull job data from a Tower server in csv form"
  :author "Anthony Green <green@redhat.com>"
  :license "AGPL3"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:file "tower-pull"))
  :depends-on (:cl-json :str :drakma :flexi-streams :local-time :chronicity :clingon :cl-date-time-parser :quri)
  :build-operation "program-op"
  :build-pathname "tower-pull"
  :entry-point "tower-pull:main")
