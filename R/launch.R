
# PIVOT: Platform for Interactive analysis and Visualization Of Transcriptomics data
# Copyright (c) 2015-2018, Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' Launch PIVOT Data Basic Analysis Module
#' @examples
#' pivot_analysis()
#' @import shiny
#' @export
pivot_main <- function(r_module = NULL) {
    Sys.setenv("R_MAX_NUM_DLLS"=180)
    cat("Gnomovision version 69, Copyright (c) 2015-2018 Qin Zhu and Junhyong Kim, University of Pennsylvania.
Gnomovision comes with ABSOLUTELY NO WARRANTY; for details go to https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html.
This is free software, and you are welcome to redistribute it under certain conditions.")
    shiny::runApp(system.file("app", package='PIVOT'),launch.browser = T)
}
