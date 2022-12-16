/**
 * @license
 * Copyright (c) 2021 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import { select, selectAll } from 'd3-selection';
import { arc, pie, line, curveLinear } from 'd3-shape';
import { scaleOrdinal, scaleLinear, scaleUtc } from 'd3-scale';
import { axisBottom, axisLeft, ticks, tickSizeOuter } from 'd3-axis';
import { max, extent, group, InternSet, range } from 'd3-array';
import { transition } from 'd3-transition';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js';
import "@polymer/paper-card/paper-card.js";
import "@polymer/paper-item/paper-icon-item.js";
import '@polymer/paper-input/paper-input.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/iron-iconset-svg/iron-iconset-svg.js';
import '@polymer/paper-styles/color.js';
import './style-element.js';

class dashBoard extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<paper-card
					id="schedGraph"
					heading="Scheduler Utilization">
				<div
						class="card-content">
					<svg
							id="schedule"
							height="500"></svg>
				</div>
			</paper-card>
			<paper-card
					heading="Table Size">
				<div
						class="card-content">
					<svg
							id="tableSize"
							width="500"
							height="200">
					</svg>
				</div>
			</paper-card>
			<paper-card
					heading="Uptime">
				<div
						class="card-content">
					<svg
							id="uptime"
							width="464"
							height="160">
					</svg>
				</div>
			</paper-card>
			<iron-ajax
					id="getHealthDashAjax"
					headers='{"Accept": "application/health+json"}'
					url="/health"
					rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true,
				value: false
			},
			tabSize: {
				type: Array,
				readOnly: true,
				notify: false,
				value: function() {
					return []
				}
			},
			uptime: {
				type: Number,
				notify: false,
				value: 0
			},
			schedulerData: {
				type: Array,
				readOnly: true,
				notify: false,
				value: function() {
					return []
				}
			},
			schedulerTimeout: {
				type: Number,
				notify: false
			}
		}
	}

	ready() {
		super.ready();
		this._healthChart();
	}

	_healthChart() {
		var ocsHealth = document.body.querySelector('sig-app')
				.shadowRoot.querySelector('sig-dashboard');
		if(ocsHealth.schedulerTimeout != "undefined") {
			clearTimeout(ocsHealth.schedulerTimeout);
		}
		var ajax = ocsHealth.shadowRoot.getElementById('getHealthDashAjax');
		var handleAjaxResponse = function(request) {
			if(request) {
				var matches = request.xhr.getResponseHeader('cache-control').match(/max-age=(\d+)/);
				var maxAge = matches ? parseInt(matches[1], 10) : -1
				var root = ocsHealth.shadowRoot;
				if(request.response){
					if(ajax.lastResponse.checks["scheduler:utilization"]) {
						for(var index in request.response.checks["scheduler:utilization"]) {
							var newRecord = new Object();
							var date = new Date();
							newRecord.dateTime = date.toISOString();
							newRecord.name = "observedValue";
							newRecord.componentId = request.response.checks
									["scheduler:utilization"][index].componentId;
							newRecord.count = request.response.checks
									["scheduler:utilization"][index].observedValue;
							ocsHealth.push('schedulerData', newRecord);
							var schedule = ocsHealth.shadowRoot.getElementById("schedule");
							var width = schedule.width.baseVal.value;
							var height = schedule.height.baseVal.value;
							var numPoints = width / 2;
							var schedulerLength = ocsHealth.schedulerData.length;
							if(schedulerLength > numPoints) {
								ocsHealth.splice('schedulerData', 0, schedulerLength - numPoints);
							}
						}
					}
				}
				var rs = getComputedStyle(document.querySelector(':root'));
				var color_size = scaleOrdinal([rs.getPropertyValue('--paper-yellow-900'),
					rs.getPropertyValue('--paper-green-900'),
					rs.getPropertyValue('--paper-red-900'),
					rs.getPropertyValue('--paper-blue-900'),
					rs.getPropertyValue('--paper-cyan-900'),
					rs.getPropertyValue('--paper-pink-900'),
					rs.getPropertyValue('--paper-teal-900'),
					rs.getPropertyValue('--paper-purple-900'),
					rs.getPropertyValue('--paper-amber-900'),
					rs.getPropertyValue('--paper-orange-900'),
					rs.getPropertyValue('--paper-light-blue-900'),
					rs.getPropertyValue('--paper-light-green-900'),
					rs.getPropertyValue('--paper-lime-900'),
					rs.getPropertyValue('--paper-blue-grey-900'),
					rs.getPropertyValue('--paper-deep-orange-900'),
					rs.getPropertyValue('--paper-deep-purple-900'),
					rs.getPropertyValue('--paper-indigo-900'),
					rs.getPropertyValue('--paper-grey-900'),
					rs.getPropertyValue('--paper-brown-900')]);
				var svgSched = select(root).select("#schedule");
				ocsHealth.draw_line(svgSched, ocsHealth.schedulerData, width, height, color_size);
				if(request.response){
					if(request.response.checks["table:size"]) {
						ocsHealth.tabSize.splice(0, ocsHealth.tabSize.length);
						for(var index in request.response.checks["table:size"]) {
							var newRecord = new Object();
							newRecord.name = request.response.checks["table:size"][index].componentId;
							newRecord.count = request.response.checks["table:size"][index].observedValue;
							ocsHealth.push('tabSize', newRecord);
						}
					}
					if(request.response.checks["uptime"]) {
						ocsHealth.uptime = request.response.checks["uptime"][0].observedValue;
					}
				}
				var svgTable = select(root).select("#tableSize");
				ocsHealth.draw_pie(svgTable, ocsHealth.tabSize, color_size);
				ocsHealth.schedulerTimeout = setTimeout(ocsHealth._healthChart, maxAge * 1000);
				var svgUp = select(root).select("#uptime");
				ocsHealth.draw_up(svgUp, ocsHealth.uptime);
			}
		}
		var handleAjaxError = function(error) {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	draw_up(svg, uptime) {
		var days = Math.floor(uptime / 86400);
		var hours = Math.floor((uptime % 86400) / 3600);
		var minutes = Math.floor((uptime % 3600) / 60);
		var g1 = svg.append('g');
		var g2 = svg.append('g');
		var g3 = svg.append('g');
		g1.append('circle')
			.attr('cx', '80')
			.attr('cy', '80')
			.attr('r', 72)
			.style('fill', '#8ac000');
		g1.append('text')
			.attr('x', '80')
			.attr('y', '80')
			.attr("text-anchor", "middle")
			.attr('dy', '10px')
			.text(days)
			.style('fill', 'white')
			.attr("font-size", '48')
			.attr("font-family", "Roboto");
		g1.append('text')
			.attr('x', '80')
			.attr('y', '80')
			.attr("text-anchor", "middle")
			.attr('dy', '1.5em')
			.text('Days')
			.style('fill', 'white')
			.attr("font-size", '24')
			.attr("font-family", "Roboto");
		g2.append('circle')
			.attr('cx', '232')
			.attr('cy', '80')
			.attr('r', 72)
			.style('fill', '#8ac000');
		g2.append('text')
			.attr('x', '232')
			.attr('y', '80')
			.attr("text-anchor", "middle")
			.attr('dy', '10px')
			.text(hours)
			.style('fill', 'white')
			.attr("font-size", '48')
			.attr("font-family", "Roboto");
		g2.append('text')
			.attr('x', '232')
			.attr('y', '80')
			.attr("text-anchor", "middle")
			.attr('dy', '1.5em')
			.text('Hours')
			.style('fill', 'white')
			.attr("font-size", '24')
			.attr("font-family", "Roboto");
		g3.append('circle')
			.attr('cx', '384')
			.attr('cy', '80')
			.attr('r', 72)
			.style('fill', '#8ac000');
		g3.append('text')
			.attr('x', '384')
			.attr('y', '80')
			.attr("text-anchor", "middle")
			.attr('dy', '10px')
			.text(minutes)
			.style('fill', 'white')
			.attr("font-size", '48')
			.attr("font-family", "Roboto");
		g3.append('text')
			.attr('x', '384')
			.attr('y', '80')
			.attr("text-anchor", "middle")
			.attr('dy', '1.5em')
			.text('Minutes')
			.style('fill', 'white')
			.attr("font-size", '24')
			.attr("font-family", "Roboto");
		return svg.node();
	}

	draw_line(svg, data, width, height, color) {
		var curve = curveLinear;
		var marginTop = 20;
		var marginRight = 30;
		var marginBottom = 30;
		var marginLeft = 40;
		var x = d => Date.parse(d.dateTime);
		var y = d => d.count;
		var z = d => parseInt(d.componentId);
		var defined;
		var xDomain;
		var yDomain;
		var zDomain;
		var xRange = [marginLeft, width - marginRight];
		var yRange = [height - marginBottom, marginTop];
		var title;
		var strokeLinecap;
		var strokeLinejoin;
		var strokeWidth = 1.5;
		var strokeOpacity;
		var mixBlendMode = "multiply";
		var yLabel = "↑ Scheduler (%)";
		var yFormat;
		svg.selectAll("*").remove();
		select("body").selectAll(".tooltip").remove();
		var X = data.map(x);
		var Y = data.map(y);
		var Z = data.map(z);
		if (defined === undefined) defined = (d, i) => !isNaN(X[i]) && !isNaN(Y[i]);
		var D = data.map(defined);
		if (xDomain === undefined) xDomain = extent(X);
		if (yDomain === undefined) yDomain = [0, max(Y)];
		if (zDomain === undefined) zDomain = Z;
		zDomain = new InternSet(zDomain);
		var I = range(X.length).filter(i => zDomain.has(Z[i]));
		var xScale = scaleUtc(xDomain, xRange);
		var yScale = scaleLinear()
			.domain(yDomain).nice()
			.range(yRange);
		var xAxis = axisBottom(xScale).ticks(width / 80).tickSizeOuter(0);
		var yAxis = axisLeft(yScale).ticks(height / 60, yFormat);
		var T = title === undefined ? Z : title === null ? null : data.map(data, title);
		var Valueline = line()
			.defined(i => D[i])
			.curve(curve)
			.x(i => xScale(X[i]))
			.y(i => yScale(Y[i]));
		svg.append("g")
			.attr("transform", `translate(${marginLeft},0)`)
			.call(yAxis)
			.call(g => g.select(".domain").remove())
			.call(g => g.append("text")
				.attr("x", -marginLeft)
				.attr("y", 10)
				.attr("fill", "currentColor")
				.attr("text-anchor", "start")
				.text(yLabel));
		var div = select("body").append("div")
			.attr("class", "tooltip")
			.style("position", "absolute")
			.style("top", "300px")
			.style("left", "20px")
			.style("z-index", "10")
			.style("opacity", 0);
		var path = svg.append("g")
			.selectAll("path")
			.data(group(I, i => Z[i]))
			.join("path")
				.style("mix-blend-mode", mixBlendMode)
				.attr("d", ([, I]) => Valueline(I))
				.attr("fill", "none")
				.attr("stroke", color)
				.attr("stroke-linecap", strokeLinecap)
				.attr("stroke-width", strokeWidth)
				.attr("stroke-opacity", strokeOpacity)
				.on('mouseover', function (d, i) {
					select(this)
						.transition()
						.duration('50')
						.attr('opacity', '.500');
					div.transition()
						.duration('50')
						.style("opacity", 1);
					var scheduler = i[0];
					var minVal = Math.min.apply(null, i[1]);
					var maxVal = Math.max.apply(null, i[1]);
					var total = i[1].reduce((acc, c) => acc + c, 0);
					var meanAve = total/i[1].length;
					var numm = "sched=" + scheduler
							+ ", min=" + minVal
							+ ", mean=" + Math.round(meanAve)
							+ ", max=" + maxVal;
					div.html(numm )
						.style("left", (d.pageX + 10) + "px")
						.style("top", (d.pageY - 15) + "px");
				})
				.on('mouseout', function (d, i) {
					select(this).transition()
						.duration('50')
						.attr('opacity', '1');
					div.transition()
						.duration('50')
						.style("opacity", 0);
				});
		var dot = svg.append("g")
			.attr("display", "none");
		dot.append("circle")
			.attr("r", 2.5);
		dot.append("text")
			.attr("font-family", "sans-serif")
			.attr("font-size", 10)
			.attr("text-anchor", "middle")
			.attr("y", -8);
	}

	draw_pie(svg, data, color) {
		svg.selectAll("*").remove();
		var g = svg.append("g");
		g.append("g")
			.attr("class", "labels");
		g.append("g")
			.attr("class", "lines");
		g.append("g")
			.attr("class", "slices");
		var width = +svg.attr('width');
		var height = +svg.attr('height');
		var radius = Math.min(width, height) / 2;
		var pie1 = pie()
			.sort(null)
			.value(function(d) {
				return d.count;
			});
		var path = arc()
			.outerRadius(radius*0.4)
			.innerRadius(radius*0.8);
		var label = arc()
			.outerRadius(radius * 0.9)
			.innerRadius(radius * 0.9);
		g.attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");
		g.select('.slices').selectAll('path')
			.data(pie1(data))
			.enter().append('path')
			.attr('d', path)
			.attr("fill", function(d) {
				return color(d.data.name)
			});
		g.select('.lines').selectAll('polyline')
			.data(pie1(data))
			.enter().append('polyline')
			.attr('points', function(d) {
				var pos = label.centroid(d);
				pos[0] = radius * 0.95 * (midAngle(d) < Math.PI ? 1 : -1);
					return [path.centroid(d), label.centroid(d), pos]
				})
			.style("fill", "none")
			.style("stroke", "black")
			.style("stroke-width", "2px");
		g.select('.labels').selectAll('text')
			.data(pie1(data))
			.enter().append('text')
			.attr('dy', '.35em')
			.attr('dx', function(d) {
				return (midAngle(d)) < Math.PI ? '0em' : '0em';
			})
			.html(function(d) {
				return d.data.name + ": " + d.data.count;
			})
			.attr('transform', function(d) {
				var pos = label.centroid(d);
				pos[0] = radius * 0.95 * (midAngle(d) < Math.PI ? 1 : -1);
				return 'translate(' + pos + ')';
			})
			.style('text-anchor', function(d) {
				return (midAngle(d)) < Math.PI ? 'start' : 'end';
			});
		function midAngle(d) {
			return d.startAngle + (d.endAngle - d.startAngle) / 2;
		};
	}
}

window.customElements.define('sig-dashboard', dashBoard);
