/**
 * Copyright 2016 - 2022 SigScale Global Inc.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js';
import '@polymer/paper-checkbox/paper-checkbox.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class offerUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateOfferModal" modal>
				<app-toolbar>
					<paper-tabs selected="{{selected}}">
						<paper-tab>
							<h2>Offering</h2>
						</paper-tab>
						<paper-tab>
							<h2>Prices</h2>
						</paper-tab>
						<paper-tab>
							<h2>Alterations</h2>
						</paper-tab>
					</paper-tabs>
				</app-toolbar>
				<paper-progress
						id="progressId"
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div>
						<div>
							<paper-input
									value="{{updateOfferName}}"
									label="Name"
									disabled>
							</paper-input>
						</div>
						<div>
							<paper-input
									value="{{updateOfferDescription}}"
									label="Description">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer description
							</paper-tooltip>
						</div>
						<div>
							<span>Bundled Products</span>
							<paper-icon-button
								id="onClickBundleUpdate"
								suffix
								icon="arrow-drop-down"
								on-click="_onClickBundleUpdate">
							</paper-icon-button>
						</div>
						<iron-collapse id="addBundleUpdate">
							<template is=dom-repeat items="{{offers}}">
								<div>
									<paper-checkbox class="bundleCheck" checked="{{item.checked}}">
										{{item.name}}
									</paper-checkbox>
								</div>
							</template>
						</iron-collapse>
						<div>
							<paper-input
									value="{{updateOfferSpecification}}"
									label="Product Specification"
									disabled>
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer specification
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									value="{{updateOfferStartDate}}"
									label="Start Date">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer start date
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									value="{{updateOfferEndDate}}"
									label="End Date">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer end date
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Status"
									value="{{offerUpdateStatus}}"
									no-animations="true">
								<paper-listbox
										id="updateOfferStatus"
										slot="dropdown-content">
									<paper-item>
										In Study
									</paper-item>
									<paper-item>
										In Design
									</paper-item>
									<paper-item>
										In Test
									</paper-item>
									<paper-item>
										Active
									</paper-item>
									<paper-item>
										Rejected
									</paper-item>
									<paper-item>
										Launched
									</paper-item>
									<paper-item>
										Retired
									</paper-item>
									<paper-item>
										Obsolete
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="updateOnClickOfferChars"
									suffix
									icon="arrow-drop-down"
									on-click="_updateOnClickOfferChars">
							</paper-icon-button>
						</div>
						<iron-collapse id="updateAddPriceOfferChars">
							<div>
								<paper-input
										id="updateReserveSessionTime"
										value="{{updateReserveSessionTime}}"
										allowed-pattern="[0-9sm]"
										pattern="^[0-9]+[sm]?$"
										auto-validate
										label="RADIUS Reserve Session Time">
								</paper-input>
								<paper-tooltip>
									Reserve an amount of time at session start
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										id="updateReserveSessionOctets"
										value="{{updateReserveSessionOctets}}"
										allowed-pattern="[0-9bkmg]"
										pattern="^[0-9]+[bkmg]?$"
										auto-validate
										label="RADIUS Reserve Session Bytes">
								</paper-input>
								<paper-tooltip>
									Reserve an amount of bytes at session start
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="Service Identifier"
										type="number"
										auto-validate
										value="{{serviceIdentifier}}">
								</paper-input>
								<paper-tooltip>
									Limit to specific service identifier
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										pattern="(^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})|(^http://.+)|(sip:.+)"
										auto-validate
										label="Redirect Server"
										value="{{updateRedirect}}">
								</paper-input>
								<paper-tooltip>
									An IPv4/IPv6 address, HTTP URL or SIP URI to which traffic will be redirected when out of credit.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										value="{{priceUpdatePolicy}}"
										type="string"
										label="Policy Table">
								</paper-input>
								<paper-tooltip>
									Add a value to update the offer policy
								</paper-tooltip>
							</div>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									autofocus
									on-tap="_updateProductOffer"
									class="update-button">
								Update
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								cancel
							</paper-button>
							<paper-button
									toggles
									raised
									on-tap="_deleteOffer"
									class="delete-button">
								Delete
							</paper-button>
						</div>
					</div>
					<div>
						<div>
							<datalist id="updatePriceNames">
								<template is="dom-repeat" items="{{prices}}">
									<option value="{{item.name}}" />
								</template>
							</datalist>
							<input id="updatePriceName"
									list="updatePriceNames"
									value="{{priceUpdateName::input}}"
									placeholder="Name"/>
							<paper-tooltip>
								Add a value to update the offer price name
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{priceUpdateDescription}}"
									label="Description">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price description
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									value="{{updateOfferStartDatePrice}}"
									label="Start Date">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price start date
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									value="{{updateOfferEndDatePrice}}"
									label="End Date">
							</paper-input>
							<paper-tooltip
								Add a value to update the offer price end date
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Price Type"
									value="{{priceUpdateType}}"
									no-animations="true"
									on-selected-item-changed="checkRecurring">
								<paper-listbox
										id="updatePriceType"
										slot="dropdown-content">
									<paper-item>
										Recurring
									</paper-item>
									<paper-item>
										One Time
									</paper-item>
									<paper-item>
										Usage
									</paper-item>
									<paper-item>
										Tariff
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Select a value to update the offer price type (Recurring | Onetime | Usage | Tariff)
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									id="updatePla"
									value="{{priceUpdatePla}}"
									input="url"
									pattern="https?://.+"
									label="Pricing Logic Algorithm"
									auto-validate>
							</paper-input>
							<paper-tooltip>
								Provide a URL to a remote (Class A) Rating Function (RF)
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									id="updatePriceSize"
									value="{{priceUpdateSize}}"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									label="Unit Size"
									auto-validate>
							</paper-input>
							<paper-tooltip>
								Unit of measure: Bytes=(MB="m", GB="g", KB="k"), Seconds=(Minutes="m",Hours="h")
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Units"
									value="{{priceUpdateUnits}}"
									no-animations="true"
									on-selected-item-changed="checkPattern">
								<paper-listbox
										id="updatePriceUnits"
										slot="dropdown-content">
									<paper-item id="priceBytes">
											Bytes
									</paper-item>
									<paper-item id="priceCents">
											Cents
									</paper-item>
									<paper-item id="priceSeconds">
											Seconds
									</paper-item>
									<paper-item id="priceMessages">
											Messages
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip
								Select a value to update the offer price unit (Bytes | Cents | Seconds)
							</paper-tooltip>
						</div>
						<div>
							<paper-input id="updatePriceAmount"
									value="{{priceUpdateAmount}}"
									type="text"
									allowed-pattern="[0-9.]"
									pattern="[0-9]+\.?[0-9]{0,6}$"
									auto-validate
									label="Amount"
									value="0">
							</paper-input>
							<paper-tooltip
								Add a value to update the offer price tax included amount
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{priceUpdateCurrency}}"
									label="Currency">
							</paper-input>
							<paper-tooltip
								Add a value to update the offer price tax currency
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu id="updatePricePerioddrop"
									value="{{priceUpdatePeriod}}"
									no-animations="true"
									label="Period">
								<paper-listbox
										id="updatePricePeriod"
										slot="dropdown-content"
										selected="2">
									<paper-item>
										Hourly
									</paper-item>
									<paper-item>
										Daily
									</paper-item>
									<paper-item>
										Weekly
									</paper-item>
									<paper-item>
										Monthly
									</paper-item>
									<paper-item>
										Yearly
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Add a value to update the offer price period (Hour | Daily | Weekly | Monthly | Yearly)
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									value="{{priceUpdateAlteration}}"
									no-animations="true"
									label="Alterations">
								<paper-listbox
										id="addUpdatePriceAlteration"
										slot="dropdown-content">
									<template is="dom-repeat" items="[[alterations]]">
										<paper-item>
											{{item.name}}
										</paper-item>
									</template>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Add a value to update the offer price alteration
							</paper-tooltip>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="updateOnClickChars"
									suffix
									icon="arrow-drop-down"
									on-click="_updateOnClickChars">
							</paper-icon-button>
						</div>
						<iron-collapse id="updateAddPriceChars">
							<div>
								<span>Time of Day Range</span>
								<paper-icon-button
										id="updateOnClickCharsTime"
										suffix
										icon="arrow-drop-down"
										on-click="_updateOnClickCharsTime">
								</paper-icon-button>
							</div>
							<iron-collapse id="updatePriceCharsTime">
								<div>
									<paper-input
											type="time"
											value="{{priceUpdateTODStart}}"
											label="Start Time">
									</paper-input>
									<paper-tooltip>
										Add a value to update the offer price start time of day
									</paper-tooltip>
								</div>
								<div>
									<paper-input
											type="time"
											value="{{priceUpdateTODEnd}}"
											label="End Time">
									</paper-input>
									<paper-tooltip>
										Add a value to update the offer price end time of day
									</paper-tooltip>
								</div>
							</iron-collapse>
							<div>
								<span>Call Direction</span>
								<paper-icon-button
										id="updateOnClickCall"
										suffix
										icon="arrow-drop-down"
										on-click="_updateOnClickCall">
								</paper-icon-button>
							</div>
							<iron-collapse
									id="updateAddCall">
								<paper-checkbox
										checked="priceUpdateCallDirIn">
									Incoming
								</paper-checkbox>
								<paper-checkbox
										checked="priceUpdateCallDirOut">
									Outgoing
								</paper-checkbox>
							</iron-collapse>
							<div>
								<paper-input
										id="priceUpdateReserveTimeInput"
										value="{{priceUpdateReserveTime}}"
										allowed-pattern="[0-9sm]"
										pattern="^[0-9]+[sm]?$"
										auto-validate
										label="RADIUS Reserve Time">
								</paper-input>
								<paper-tooltip>
									Add a value to update the offer price characteristic reserve time
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										id="priceUpdateReserveOctetsInput"
										value="{{priceUpdateReserveOctets}}"
										allowed-pattern="[0-9bkmg]"
										pattern="^[0-9]+[bkmg]?$"
										auto-validate
										label="RADIUS Reserve Data">
								</paper-input>
								<paper-tooltip
									Add a value to update the offer price characteristic reserve bytes
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										value="{{priceUpdateTariff}}"
										type="string"
										label="Prefix Tariff Table">
								</paper-input>
								<paper-tooltip>
									Add a value to update the offer price destination prefix tariff
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										type="string"
										value="{{priceUpdateRoamingTable}}"
										label="Roaming Table">
								</paper-input>
								<paper-tooltip>
									Add a value to update the offer price roaming table
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										value="{{priceUpdateChargingKey}}"
										type="number"
										label="Charging Key">
								</paper-input>
								<paper-tooltip>
									Limit to a specific charging key (rating group)
								</paper-tooltip>
							</div>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									autofocus
									id="updateOfferPriceButton"
									on-tap="_updateOfferPrice"
									class="update-button"
									hidden>
								Update
							</paper-button>
							<paper-button
									raised
									id="updateOfferAddButton"
									class="submit-button"
									on-tap="_updateAddPrice">
								Add
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								Cancel
							</paper-button>
						</div>
					</div>
					<div">
						<div>
							<datalist id="updateAltNames">
								<template is="dom-repeat" items="{{alterations}}">
									<option value="{{item.name}}"/>
								</template>
							</datalist>
							<input id="updateAltName"
									list="updateAltNames"
									value="{{altUpdateName::input}}"
									placeholder="Name"/>
							<paper-tooltip>
								Add a value to update the offer price alteration name
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{altUpdateDescription}}"
									label="Description">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price alteration description
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									value="{{updateOfferStartDateAlt}}"
									label="Start Date">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price alteration start date
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									value="{{updateOfferEndDateAlt}}"
									label="End Date">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price alteration end date
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Price Type"
									value="{{altUpdateType}}"
									no-animations="true"
									on-selected-item-changed="checkRecurringAlt">
								<paper-listbox
										id="updateAltType"
										slot="dropdown-content">
									<paper-item>
										Recurring
									</paper-item>
									<paper-item>
										One Time
									</paper-item>
									<paper-item>
										Usage
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip
								Add a value to update the offer price alteration type (Recurring | Onetime | Usage)
							</paper-tooltip>
						</div>
						<div>
							<paper-input id="updateAltSize"
									label="Unit Size"
									type="text"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									auto-validate
									value=1>
							</paper-input>
							<paper-tooltip
								Unit of measure: Bytes=(MB="m", GB="g", KB="k"), Seconds=(Minutes="m",Hours="h")
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									id="updateAltsUnitsdrop"
									value= "{{altUpdateUnits}}"
									no-animations="true"
									label="Units">
								<paper-listbox
										id="updateAlterationUnits"
										on-selected-item-changed="checkPatternAlt"
										slot="dropdown-content">
									<paper-item id="altBytes">
											Bytes
									</paper-item>
									<paper-item id="altCents">
											Cents
									</paper-item>
									<paper-item id="altSeconds">
											Seconds
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Select a value to update the offer price alteration unit (Bytes | Cents | Seconds)
							</paper-tooltip>
						</div>
						<div>
							<paper-input id="updateAltAmount"
									label="Amount"
									type="text"
									allowed-pattern="[0-9.]"
									pattern="[0-9]+\.?[0-9]{0,6}$"
									auto-validate
									value=0>
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price alteration tax included amount
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{altUpdateCurrency}}"
									label="Currency">
							</paper-input>
							<paper-tooltip>
								Add a value to update the offer price alteration currency
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu id="addAltPeriodDrop"
									label="Period"
									no-animations="true"
									value="{{altUpdatePeriod}}">
								<paper-listbox
										id="updateAltPeriod"
										slot="dropdown-content"
										selected="2">
									<paper-item>
										Hourly
									</paper-item>
									<paper-item>
										Daily
									</paper-item>
									<paper-item>
										Weekly
									</paper-item>
									<paper-item>
										Monthly
									</paper-item>
									<paper-item>
										Yearly
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Add a value to update the offer price alteration period (Hour | Daily | Weekly | Monthly | Yearly)
							</paper-tooltip>
						</div>
						<div class="buttons">
							<paper-button
									id="updateOfferAlterationButton"
									autofocus
									on-tap="_updateOfferAlteration"
									class="update-button"
									hidden>
								Update
							</paper-button>
							<paper-button
									raised
									id="updateAddAlterationButton"
									class="submit-button"
									on-tap="_updateAddAlteration">
								Add
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								cancel
							</paper-button>
						</div>
					</div>
				</iron-pages>
			</paper-dialog>
			<iron-ajax
					id="updateProductOfferAjax"
					method="PATCH"
					content-type="application/json-patch+json"
					on-loading-changed="_onLoadingChanged"
					on-response="_updateProductOfferResponse"
					on-error="_updateProductOfferError">
			</iron-ajax>
			<iron-ajax
					id="updateOfferPriceAjax"
					on-response="_updateOfferPriceResponse"
					on-error="_updateOfferPriceError">
			</iron-ajax>
			<iron-ajax
					id="deleteOfferAjax"
					method="DELETE"
					on-response="_deleteOfferResponse"
					on-error="_deleteOfferError">
			</iron-ajax>
			<iron-ajax
					id="updateOfferAlterationAjax"
					on-response="_updateOfferAlterationResponse"
					on-error="_updateOfferAlterationError">
			</iron-ajax>
			<iron-ajax
					id="getOffersAjax"
					url="/catalogManagement/v2/productOffering"
					on-response="_getProductUpdateResponse"
					on-error="_getProductsError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			priceUpdateName: {
				observer: '_updatePricesDialog'
			},
			altUpdateName: {
				observer: '_updateAlterationsDialog'
			},
			alterations: {
				type: Array,
				value: function() {
					return [];
				}
			},
			prices: {
				type: Array,
				value: function() {
					return [];
				}
			},
			offers: {
				type: Array,
				value: function() {
					return [];
				}
			},
			loading: {
				type: Boolean,
				value: false
			},
			selected: {
				type: Number,
				value: 0
			},
			priceUpdatePolicy: {
				type: String
			},
			updateOfferName: {
				type: String
			},
			updateOfferDescription: {
				type: String
			},
			updateOfferSpecification: {
				type: String
			},
			updateRedirect: {
				type: String
			},
			serviceIdentifier: {
				type: Number
			},
			priceUpdateDescription: {
				type: String
			},
			updateOfferStartDate: {
				type: String
			},
			updateOfferEndDate: {
				type: String
			},
			updateOfferStartDatePrice: {
				type: String
			},
			updateOfferEndDatePrice: {
				type: String
			},
			priceUpdatePla: {
				type: String
			},
			priceUpdateSize: {
				type: String
			},
			priceUpdateCurrency: {
				type: String
			},
			priceUpdateTODStart: {
				type: String
			},
			priceUpdateTODEnd: {
				type: String
			},
			priceUpdateCallDirIn: {
				type: Boolean
			},
			priceUpdateCallDirOut: {
				type: Boolean
			},
			priceUpdateTariff: {
				type: String
			},
			priceUpdateChargingKey: {
				type: Number
			},
			altUpdateDescription: {
				type: String
			},
			updateOfferStartDateAlt: {
				type: String
			},
			updateOfferEndDateAlt: {
				type: String
			},
			altUpdateCurrency: {
				type: String
			},
			priceUpdateReserveTime: {
				type: String
			},
			priceUpdateReserveOctets: {
				type: String
			},
			updateReserveSessionTime: {
				type: String
			},
			updateReserveSessionOctets: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var current;
			if(item) {
				current = item;
			} else if(last) {
				current = last;
			}
			this.$.getOffersAjax.generateRequest();
			this.$.updateOfferModal.open();
			this.updateOfferName = current.id;
			this.updateOfferDescription = current.description;
			if(current.productSpecification && current.productSpecification.name) {
				this.updateOfferSpecification = current.productSpecification.name.replace("ProductSpec", "");
			}
			var date;
			if(current.startDate) {
				date = new Date(current.startDate);
				this.set('updateOfferStartDate', date.toISOString().slice(0, -1));
			}
			if(current.endDate) {
				date = new Date(current.endDate);
				this.set('updateOfferEndDate', date.toISOString().slice(0, -1));
			}
			this.offerUpdateStatus = current.lifecycleStatus;
			for (var indexCha in current.prodSpecCharValueUse) {
				if(current.prodSpecCharValueUse[indexCha].name == "radiusReserveSessionTime") {
					var rst = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0];
					if(rst.unitOfMeasure && rst.unitOfMeasure.length > 0) {
						if(rst.unitOfMeasure == "seconds") {
							this.updateReserveSessionTime = rst.value;
						} else if(rst.unitOfMeasure == "minutes") {
							this.updateReserveSessionTime = rst.value + "m";;
						}
					} else {
						this.updateReserveSessionTime = rst.value;
					}
					if(this.updateReserveSessionTime.length >= 0) {
						this.$.updateReserveSessionOctets.disabled = true;
					}
				}
				if(current.prodSpecCharValueUse[indexCha].name == "radiusReserveSessionOctets") {
					var rso = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0];
					if(rso.unitOfMeasure && rso.unitOfMeasure.length > 0) {
						if(rso.unitOfMeasure == "bytes") {
							this.updateReserveSessionOctets = rso.value;
						} else if(rso.unitOfMeasure == "kilobytes") {
							this.updateReserveSessionOctets = rso.value + "k";;
						} else if(rso.unitOfMeasure == "megabytes") {
							this.updateReserveSessionOctets = rso.value + "m";;
						} else if(rso.unitOfMeasure == "gigabytes") {
							this.updateReserveSessionOctets = rso.value + "g";;
						}
					} else {
						this.updateReserveSessionOctetse = rso.value;
					}
					if(this.updateReserveSessionOctets.length >= 0) {
						this.$.updateReserveSessionTime.disabled = true;
					}
				}
				if(current.prodSpecCharValueUse[indexCha].name == "redirectServer") {
					this.updateRedirect = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
				if(current.prodSpecCharValueUse[indexCha].name == "serviceIdentifier") {
					this.serviceIdentifier = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
				if(current.prodSpecCharValueUse[indexCha].name == "policyTable") {
					this.priceUpdatePolicy = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
			}
			this.splice('prices', 0);
			for(var index in current.prices) {
				var newPrice = new Object();
				newPrice.name = current.prices[index].name;
				newPrice.description = current.prices[index].description;
				if(current.prices[index].validFor) {
					if(current.prices[index].validFor.startDateTime) {
						date = new Date(current.prices[index].validFor.startDateTime);
						newPrice.start = date.toISOString().slice(0, -1);
					}
					if(current.prices[index].validFor.endDateTime) {
						date = new Date(current.prices[index].validFor.endDateTime);
						newPrice.end = date.toISOString().slice(0, -1);
					}
				}
				switch(current.prices[index].priceType) {
					case "recurring":
						newPrice.priceType = "Recurring";
						break;
					case "one_time":
						newPrice.priceType = "One Time";
						break;
					case "usage":
						newPrice.priceType = "Usage";
						break;
					case "tariff":
						newPrice.priceType = "Tariff";
						break;
				}
				if (current.prices[index].pricingLogicAlgorithm) {
					newPrice.pla = current.prices[index].pricingLogicAlgorithm.pop().href;
				}
				if (current.prices[index].unitOfMeasure) {
					var unitOfMeasure = current.prices[index].unitOfMeasure;
					switch(unitOfMeasure.slice(-1)) {
						case "b":
							var conv;
							conv = unitOfMeasure = parseInt(unitOfMeasure.slice(0, -1));
							newPrice.size = conv.toString();
							newPrice.unit = "b";
							break;
						case "s":
							var conv1;
							conv1 = parseInt(unitOfMeasure.slice(0, -1));
							newPrice.size = conv1.toString();
							newPrice.unit = "s";
							break;
						case "msg":
							var conv2;
							conv2 = parseInt(unitOfMeasure.slice(0, -1));
							newPrice.size = conv2.toString();
							newPrice.unit = "msg";
							break;
					}
				}
				if(current.prices[index].price) {
					newPrice.currency = current.prices[index].price.currencyCode;
					newPrice.amount = current.prices[index].price.taxIncludedAmount;
				}
				var prodPrice = current.prices[index];
				if(prodPrice.prodSpecCharValueUse) {
					var specChar = new Array();
					for (var indexChar in prodPrice.prodSpecCharValueUse) {
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "radiusReserveTime") {
							specChar[indexChar] = {name: "radiusReserveTime",
									unitOfMeasure: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].unitOfMeasure,
									value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "radiusReserveOctets") {
							specChar[indexChar] = {name: "radiusReserveOctets",
									unitOfMeasure: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].unitOfMeasure,
									value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "timeOfDayRange") {
							specChar[indexChar] = {name: "timeOfDayRange", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "callDirection") {
							specChar[indexChar] = {name: "callDirection", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "destPrefixTariffTable") {
							specChar[indexChar] = {name: "destPrefixTariffTable", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "roamingTable") {
							specChar[indexChar] = {name: "roamingTable", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "chargingKey") {
							specChar[indexChar] = {name: "chargingKey", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
					}
				}
				newPrice.prodSpecCharValueUse = specChar;
				switch(current.prices[index].recurringChargePeriod) {
					case "hourly":
						newPrice.period = "Hourly";
						break;
					case "daily":
						newPrice.period = "Daily";
						break;
					case "weekly":
						newPrice.period = "Weekly";
						break;
					case "monthly":
						newPrice.period = "Monthly";
						break;
					case "yearly":
						newPrice.period = "Yearly";
						break;
				}
				if(current.prices[index].productOfferPriceAlteration) {
					var altName = current.prices[index].productOfferPriceAlteration.name;
					function checkAlt1(alt) {
						return alt.name == altName;
					}
					var altIndex = this.alterations.findIndex(checkAlt1);
					if(altIndex == -1) {
						var newAlt = new Object();
						newAlt.name = current.prices[index].productOfferPriceAlteration.name;
						newAlt.description = current.prices[index].productOfferPriceAlteration.description;
						if(current.prices[index].validFor) {
							newAlt.start = current.prices[index].productOfferPriceAlteration.validFor.startDateTime;
							newAlt.end = current.prices[index].productOfferPriceAlteration.validFor.endDateTime;
						}
						switch(current.prices[index].productOfferPriceAlteration.priceType) {
							case "recurring":
								newAlt.priceType = "Recurring";
								break;
							case "one_time":
								newAlt.priceType = "One Time";
								break;
							case "usage":
								newAlt.priceType = "Usage";
								break;
						}
						if (current.prices[index].productOfferPriceAlteration.unitOfMeasure) {
							var unitOfMeasure = current.prices[index].productOfferPriceAlteration.unitOfMeasure;
							switch(unitOfMeasure.slice(-1)) {
								case "b":
									newAlt.size = parseInt(unitOfMeasure.slice(0, -1));
									newAlt.unit = "b";
									break;
								case "s":
									newAlt.size = parseInt(unitOfMeasure.slice(0, -1));
									newAlt.unit = "s";
									break;
								default:
									newAlt.unit = "c";
							}
						}
						if(current.prices[index].productOfferPriceAlteration.price) {
							newAlt.currency = current.prices[index].productOfferPriceAlteration.price.currencyCode;
							newAlt.amount = current.prices[index].productOfferPriceAlteration.price.taxIncludedAmount;
						}
						switch(current.prices[index].productOfferPriceAlteration.recurringChargePeriod) {
							case "hourly":
								newAlt.period = "Hourly";
								break;
							case "daily":
								newAlt.period = "Daily";
								break;
							case "weekly":
								newAlt.period = "Weekly";
								break;
							case "monthly":
								newAlt.period = "Monthly";
								break;
							case "yearly":
								newAlt.period = "Yearly";
								break;
						}
						this.push('alterations', newAlt);
						newPrice.alteration = newAlt.name;
					}
				}
				this.push('prices', newPrice);
			}
		}
	}

	_getProductUpdateResponse(event) {
		var results = event.detail.xhr.response;
		function checkExist(spec) {
			return spec.name == results[index].name;
		}
		for (var index in results) {
			if(!this.offers.some(checkExist)) {
				var offer = new Object();
				offer.id = results[index].id;
				offer.href = results[index].href;
				offer.name = results[index].name;
				offer.checked = false;
				this.push('offers', offer);
			}
		}
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_getProductsError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_updateOnClickOfferChars() {
		if(this.$.updateAddPriceOfferChars.opened == false) {
			this.$.updateAddPriceOfferChars.show();
			this.$.updateOnClickOfferChars.icon="arrow-drop-up"
			this.$.updateOfferModal.notifyResize();
		} else {
			this.$.updateAddPriceOfferChars.hide();
			this.$.updateOnClickOfferChars.icon="arrow-drop-down"
			this.$.updateOfferModal.notifyResize();
		}
	}

	_updateOnClickChars() {
		if(this.$.updateAddPriceChars.opened == false) {
			this.$.updateAddPriceChars.show();
			this.$.updateOnClickChars.icon="arrow-drop-up"
		} else {
			this.$.updateAddPriceChars.hide();
			this.$.updateOnClickChars.icon="arrow-drop-down"
		}
	}

	_updateOnClickCharsTime() {
		if(this.$.updatePriceCharsTime.opened == false) {
			this.$.updatePriceCharsTime.show();
			this.$.updateOnClickCharsTime.icon="arrow-drop-up"
		} else {
			this.$.updatePriceCharsTime.hide();
			this.$.updateOnClickCharsTime.icon="arrow-drop-down"
		}
	}

	_updateOnClickCall() {
		if(this.$.updateAddCall.opened == false) {
			this.$.updateAddCall.show();
			this.$.updateOnClickCall.icon="arrow-drop-up"
		} else {
			this.$.updateAddCall.hide();
			this.$.updateOnClickCall.icon="arrow-drop-down"
		}
	}

	_onClickBundleUpdate() {
		if(this.$.addBundleUpdate.opened == false) {
			this.$.addBundleUpdate.show();
			this.$.onClickBundleUpdate.icon="arrow-drop-up";
		} else {
			this.$.addBundleUpdate.hide();
			this.$.onClickBundleUpdate.icon="arrow-drop-down";
		}
	}

	_updatePricesDialog() {
		function checkUpdatePriceName(updatePrice) {
			return updatePrice.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		if(this.prices != undefined) {
			var indexUpdatePrice = this.prices.findIndex(checkUpdatePriceName);
			if(indexUpdatePrice != -1) {
				this.$.updateOfferAddButton.hidden = true;
				this.$.updateOfferPriceButton.hidden = false;
			} else {
				this.$.updateOfferAddButton.hidden = false;
				this.$.updateOfferPriceButton.hidden = true;
			}
			if (indexUpdatePrice == -1) {
				this.priceUpdateDescription = null;
				this.updateOfferStartDatePrice = null;
				this.updateOfferEndDatePrice = null;
				this.priceUpdateType = null;
				if(this.updateOfferSpecification == "PrepaidData") {
					this.$.priceBytes.disabled = false;
					this.$.priceSeconds.disabled = false;
					this.$.updatePriceUnits.selected = 0;
				}
				if(this.updateOfferSpecification == "PrepaidVoice") {
					this.$.priceSeconds.disabled = false;
					this.$.priceBytes.disabled = true;
					this.$.updatePriceUnits.selected = 2;
				}
				this.priceUpdatePla = null;
				this.priceUpdateSize = null;
				this.priceUpdateUnits = null;
				this.priceUpdateAmount = null;
				this.priceUpdateCurrency = null;
				this.priceUpdatePeriod = null;
				this.priceUpdateReserveTime = null;
				this.$.priceUpdateReserveTimeInput.disabled = false;
				this.priceUpdateReserveOctets = null;
				this.$.priceUpdateReserveOctetsInput.disabled = false;
				this.updateReserveSessionTime = null;
				this.$.updateReserveSessionTime.disabled = false;
				this.updateReserveSessionOctets = null;
				this.$.updateReserveSessionOctets.disabled = false;
				this.updateRedirect = null;
				this.serviceIdentifier = null;
				this.priceUpdateRoamingTable = null;
				this.priceUpdateChargingKey = null;
				this.priceUpdateTariff = null;
				this.priceUpdatePolicy = null;
				this.priceUpdateTODStart = null;
				this.priceUpdateTODEnd = null;
				this.priceUpdateCallDirIn = false;
				this.priceUpdateCallDirOut = false;
				this.priceUpdateAlteration = null;
			} else {
				this.priceUpdateDescription = this.prices[indexUpdatePrice].description;
				this.updateOfferStartDatePrice = this.prices[indexUpdatePrice].start;
				this.updateOfferEndDatePrice = this.prices[indexUpdatePrice].end;
				this.priceUpdateType = this.prices[indexUpdatePrice].priceType;
				this.priceUpdatePla = this.prices[indexUpdatePrice].pla;
				this.priceUpdateSize = this.prices[indexUpdatePrice].size;
				switch(this.prices[indexUpdatePrice].unit) {
					case "b":
						this.priceUpdateUnits = "Bytes";
						break;
					case "c":
						this.priceUpdateUnits = "Cents";
						break;
					case "s":
						this.priceUpdateUnits = "Seconds";
						break;
					case "msg":
						this.priceUpdateUnits = "Messages";
						break;
				}
				if(this.prices[indexUpdatePrice].currency) {
					this.priceUpdateCurrency = this.prices[indexUpdatePrice].currency;
				}
				if(this.prices[indexUpdatePrice].amount) {
					this.priceUpdateAmount = this.prices[indexUpdatePrice].amount;
				}
				this.priceUpdatePeriod = this.prices[indexUpdatePrice].period;
				this.priceUpdateAlteration = this.prices[indexUpdatePrice].alteration;
				var prodPriceUpdate = this.prices[indexUpdatePrice];
				if(prodPriceUpdate.prodSpecCharValueUse) {
					for (var indexCharVal in prodPriceUpdate.prodSpecCharValueUse) {
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "radiusReserveTime") {
							var rrtCharValue = prodPriceUpdate.prodSpecCharValueUse[indexCharVal];
							if(rrtCharValue.unitOfMeasure == "seconds") {
								this.priceUpdateReserveTime = rrtCharValue.value;
							} else if(rrtCharValue.unitOfMeasure == "minutes") {
								this.priceUpdateReserveTime = rrtCharValue.value + "m";
							} else {
								this.priceUpdateReserveTime = rrtCharValue.value;
							}
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "radiusReserveOctets") {
							var rroCharValue = prodPriceUpdate.prodSpecCharValueUse[indexCharVal];
							if(rroCharValue.unitOfMeasure == "bytes") {
								this.priceUpdateReserveOctets = rroCharValue.value;
							} else if(rroCharValue.unitOfMeasure == "kilobytes") {
								this.priceUpdateReserveOctets = rroCharValue.value + "k";
							} else if(rroCharValue.unitOfMeasure == "megabytes") {
								this.priceUpdateReserveOctets = rroCharValue.value + "m";
							} else if(rroCharValue.unitOfMeasure == "gigabytes") {
								this.priceUpdateReserveOctets = rroCharValue.value + "g";
							} else {
								this.priceUpdateReserveOctets = rroCharValue.value;
							}
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "destPrefixTariffTable") {
							this.priceUpdateTariff = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "roamingTable") {
							this.priceUpdateRoamingTable = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "chargingKey") {
							this.priceUpdateChargingKey = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "timeOfDayRange") {
							if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.lowerValue.units == "minutes") {
								var todStart = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.lowerValue.amount;
								this.priceUpdateTODStart = (todStart / 60).toFixed(0).padStart(2, '0')
										+ ':' + (todStart % 60).toString().padStart(2, '0');
							}
							if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.upperValue.units == "minutes") {
								var todEnd = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.upperValue.amount;
								this.priceUpdateTODEnd = (todEnd / 60).toFixed(0).padStart(2, '0')
										+ ':' + (todEnd % 60).toString().padStart(2, '0');
							}
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "callDirection") {
							if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value == "originate") {
								this.priceUpdateCallDirOut = true;
							} else if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value == "answer") {
								this.priceUpdateCallDirIn = true;
							}
						}
					}
				}
			}
		}
	}

	_updateAlterationsDialog() {
		if(this.alterations != undefined) {
			function checkUpdateAltName(updateAlts) {
				return updateAlts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;
			}
			var indexAlt = this.alterations.findIndex(checkUpdateAltName);
			if(indexAlt != -1) {
				this.$.updateAddAlterationButton.hidden = true;
				this.$.updateOfferAlterationButton.hidden = false;
			} else {
				this.$.updateAddAlterationButton.hidden = false;
				this.$.updateOfferAlterationButton.hidden = true;
			}
			if (indexAlt == -1) {
				this.altUpdateDescription = null;
				this.updateOfferStartDateAlt = null;
				this.updateOfferEndDateAlt = null;
				this.altUpdateType = null;
				if(this.updateOfferSpecification == "PrepaidData") {
					this.$.altBytes.disabled = false;
					this.$.altSeconds.disabled = false;
					this.$.updateAlterationUnits.selected = 0;
				}
				if(this.updateOfferSpecification == "PrepaidVoice") {
					this.$.altSeconds.disabled = false;
					this.$.altBytes.disabled = true;
					this.$.updateAlterationUnits.selected = 2;
				}
				this.$.updateAltSize.value = null;
				this.altUpdateUnits = null;
				this.$.updateAltAmount.value = null;
				this.altUpdateCurrency = null;
				this.altUpdatePeriod = null;
			} else {
				this.altUpdateDescription = this.alterations[indexAlt].description;
				if(this.alterations[indexAlt].start || this.alterations[indexAlt].end) {
					this.updateOfferStartDateAlt = this.alterations[indexAlt].start;
					this.updateOfferEndDateAlt = this.alterations[indexAlt].end;
				}
				this.altUpdateType = this.alterations[indexAlt].priceType;
				this.$.updateAltSize.value = this.alterations[indexAlt].size;
				switch(this.alterations[indexAlt].unit) {
					case "b":
						this.$.updateAlterationUnits.selected = 0;
						break;
					case "c":
						this.$.updateAlterationUnits.selected = 1;
						break;
					case "s":
						this.$.updateAlterationUnits.selected = 2;
						break;
				}
				if(this.alterations[indexAlt].currency || this.alterations[indexAlt].amount) {
					this.altUpdateCurrency = this.alterations[indexAlt].currency;
					this.$.updateAltAmount.value = this.alterations[indexAlt].amount;
				}
				this.altUpdatePeriod = this.alterations[indexAlt].period;
			}
		}
	}

	_updateProductOffer(event) {
		var ajax =  this.$.updateProductOfferAjax;
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName;
		var offerNew = new Array();
		if(!this.updateOfferDescription && this.activeItem.description) {
			var offerDesc = new Object();
			offerDesc.op = "remove";
			offerDesc.path = "/description";
			offerNew.push(offerDesc);
		} else if(this.updateOfferDescription && !this.activeItem.description) {
			var offerDesc = new Object();
			offerDesc.op = "add";
			offerDesc.path = "/description";
			offerDesc.value = this.updateOfferDescription;
			offerNew.push(offerDesc);
		} else if(this.updateOfferDescription != this.activeItem.description) {
			var offerDesc = new Object();
			offerDesc.op = "replace";
			offerDesc.path = "/description";
			offerDesc.value = this.updateOfferDescription;
			offerNew.push(offerDesc);
		}
		if(!this.updateOfferStartDate && !this.updateOfferEndDate
				&& (this.activeItem.startDate || this.activeItem.endDate)) {
			var validity = new Object();
			validity.op = "remove";
			validity.path = "/validFor";
			offerNew.push(validity);
		} else if((this.updateOfferStartDate || this.updateOfferEndDate)
				&& !this.activeItem.startDate && !this.activeItem.endDate) {
			var validity = new Object();
			var period = new Object();
			validity.op = "add";
			validity.path = "/validFor";
			if(this.updateOfferStartDate) {
				period.startDateTime = this.updateOfferStartDate;
			}
			if(this.updateOfferEndDate) {
				period.endDateTime = this.updateOfferEndDate;
			}
			validity.value = period;
			offerNew.push(validity);
		} else if(this.updateOfferStartDate || this.updateOfferEndDate) {
			if(!this.updateOfferStartDate && this.activeItem.startDate) {
				var startDateTime = new Object();
				startDateTime.op = "remove";
				startDateTime.path = "/validFor/startDateTime";
				offerNew.push(startDateTime);
			} else if(this.updateOfferStartDate && !this.activeItem.startDate) {
				var startDateTime = new Object();
				startDateTime.op = "add";
				startDateTime.path = "/validFor/startDateTime";
				startDateTime.value = this.updateOfferStartDate;
				offerNew.push(startDateTime);
			} else if(this.activeItem.startDate
					&& !this.updateOfferStartDate.startsWith(this.activeItem.startDate)) {
				var startDateTime = new Object();
				startDateTime.op = "replace";
				startDateTime.path = "/validFor/startDateTime";
				startDateTime.value = this.updateOfferStartDate;
				offerNew.push(startDateTime);
			}
			if(!this.updateOfferEndDate && this.activeItem.endDate) {
				var endDateTime = new Object();
				endDateTime.op = "remove";
				endDateTime.path = "/validFor/endDateTime";
				offerNew.push(endDateTime);
			} else if(this.updateOfferEndDate && !this.activeItem.endDate) {
				var endDateTime = new Object();
				endDateTime.op = "add";
				endDateTime.path = "/validFor/endDateTime";
				endDateTime.value = this.updateOfferEndDate;
				offerNew.push(endDateTime);
			} else if(this.activeItem.endDate
					&& !this.updateOfferEndDate.startsWith(this.activeItem.endDate)) {
				var endDateTime = new Object();
				endDateTime.op = "replace";
				endDateTime.path = "/validFor/endDateTime";
				endDateTime.value = this.updateOfferEndDate;
				offerNew.push(endDateTime);
			}
		}
		if(!this.offerUpdateStatus && this.activeItem.lifecycleStatus) {
			var stat = new Object();
			stat.op = "remove";
			stat.path = "/lifecycleStatus";
			offerNew.push(stat);
		} else if(this.offerUpdateStatus && !this.activeItem.lifecycleStatus) {
			var stat = new Object();
			stat.op = "add";
			stat.path = "/lifecycleStatus";
			stat.value = this.offerUpdateStatus;
			offerNew.push(stat);
		} else if(this.offerUpdateStatus !== this.activeItem.lifecycleStatus) {
			var stat = new Object();
			stat.op = "replace";
			stat.path = "/lifecycleStatus";
			stat.value = this.offerUpdateStatus;
			offerNew.push(stat);
		}
		if(this.activeItem.prodSpecCharValueUse) {
			function rstCheckName(charVal) {
				return charVal.name == "radiusReserveSessionTime";
			}
			var rstIndex = this.activeItem.prodSpecCharValueUse.findIndex(rstCheckName);
		} else {
			var rstIndex = -1;
		}
		if(!this.updateReserveSessionTime && (rstIndex != -1)) {
			var reserveSessionTime = new Object();
			reserveSessionTime.path = "/prodSpecCharValueUse/" + rstIndex;
			reserveSessionTime.op = "remove";
			offerNew.push(reserveSessionTime);
		} else {
			if(this.updateReserveSessionTime && rstIndex == -1) {
				var reserveSessionTime = new Object();
				reserveSessionTime.path = "/prodSpecCharValueUse/-";
				reserveSessionTime.op = "add";
				var rstCharValueUse = new Object();
				rstCharValueUse.name = "radiusReserveSessionTime";
				rstCharValueUse.minCardinality = 0;
				rstCharValueUse.maxCardinality = 1;
				var rstCharValueArray  = new Array();
				var rstCharValue = this.timeCharValue(this.updateReserveSessionTime);
				rstCharValue.default = true;
				rstCharValueArray.push(rstCharValue);
				rstCharValueUse.productSpecCharacteristicValue = rstCharValueArray;
				var rstProductSpec = new Object();
				rstProductSpec.id = "1";
				rstProductSpec.href = "/catalogManagement/v2/productSpecification/1";
				rstCharValueUse.productSpecification = rstProductSpec;
				reserveSessionTime.value = rstCharValueUse;
				offerNew.push(reserveSessionTime);
			} else if(this.updateReserveSessionTime && (rstIndex != -1)
					&& this.activeItem.prodSpecCharValueUse[rstIndex].productSpecCharacteristicValue
					&& this.activeItem.prodSpecCharValueUse[rstIndex].productSpecCharacteristicValue[0].value) {
				var rstCharValue = this.timeCharValue(this.updateReserveSessionTime);
				if((rstCharValue.value != this.activeItem.prodSpecCharValueUse[rstIndex].productSpecCharacteristicValue[0].value)
						|| (rstCharValue.unitOfMeasure != this.activeItem.prodSpecCharValueUse[rstIndex].productSpecCharacteristicValue[0].unitOfMeasure)) {
					var reserveSessionTime = new Object();
					reserveSessionTime.path = "/prodSpecCharValueUse/"
							+ rstIndex + "/productSpecCharacteristicValue/0/";
					reserveSessionTime.op = "replace";
					reserveSessionTime.value = rstCharValue;
					offerNew.push(reserveSessionTime);
				}
			}
		}
		if(this.activeItem.prodSpecCharValueUse) {
			function rsoCheckName(charVal) {
				return charVal.name == "radiusReserveSessionOctets";
			}
			var rsoIndex = this.activeItem.prodSpecCharValueUse.findIndex(rsoCheckName);
		} else {
			var rsoIndex = -1;
		}
		if(!this.updateReserveSessionOctets && (rsoIndex != -1)) {
			var reserveSessionOctets = new Object();
			reserveSessionOctets.path = "/prodSpecCharValueUse/" + rsoIndex;
			reserveSessionOctets.op = "remove";
			offerNew.push(reserveSessionOctets);
		} else {
			if(this.updateReserveSessionOctets && rsoIndex == -1) {
				var reserveSessionOctets = new Object();
				reserveSessionOctets.path = "/prodSpecCharValueUse/-";
				reserveSessionOctets.op = "add";
				var rsoCharValueUse = new Object();
				rsoCharValueUse.name = "radiusReserveSessionOctets";
				rsoCharValueUse.minCardinality = 0;
				rsoCharValueUse.maxCardinality = 1;
				var rsoCharValueArray  = new Array();
				var rsoCharValue = this.octetsCharValue(this.updateReserveSessionOctets);
				rsoCharValue.default = true;
				rsoCharValueArray.push(rsoCharValue);
				rsoCharValueUse.productSpecCharacteristicValue = rsoCharValueArray;
				var rsoProductSpec = new Object();
				rsoProductSpec.id = "1";
				rsoProductSpec.href = "/catalogManagement/v2/productSpecification/1";
				rsoCharValueUse.productSpecification = rsoProductSpec;
				reserveSessionOctets.value = rsoCharValueUse;
				offerNew.push(reserveSessionOctets);
			} else if(this.updateReserveSessionOctets && (rsoIndex != -1)
					&& this.activeItem.prodSpecCharValueUse[rsoIndex].productSpecCharacteristicValue
					&& this.activeItem.prodSpecCharValueUse[rsoIndex].productSpecCharacteristicValue[0].value) {
				var rsoCharValue = this.octetsCharValue(this.updateReserveSessionOctets);
				if((rsoCharValue.value != this.activeItem.prodSpecCharValueUse[rsoIndex].productSpecCharacteristicValue[0].value)
						|| (rsoCharValue.unitOfMeasure != this.activeItem.prodSpecCharValueUse[rsoIndex].productSpecCharacteristicValue[0].unitOfMeasure)) {
					var reserveSessionOctets = new Object();
					reserveSessionOctets.path = "/prodSpecCharValueUse/"
							+ rsoIndex + "/productSpecCharacteristicValue/0";
					reserveSessionOctets.op = "replace";
					reserveSessionOctets.value = rsoCharValue;
					offerNew.push(reserveSessionOctets);
				}
			}
		}
		if(this.activeItem.prodSpecCharValueUse) {
			function redCheckName(charVal) {
				return charVal.name == "redirectServer";
			}
			var redIndex = this.activeItem.prodSpecCharValueUse.findIndex(redCheckName);
		} else {
			var redIndex = -1;
		}
		if(!this.updateRedirect && (redIndex != -1)) {
			var redirectServer = new Object();
			redirectServer.path = "/prodSpecCharValueUse/" + redIndex;
			redirectServer.op = "remove";
			offerNew.push(redirectServer);
		} else if(this.updateRedirect && (redIndex == -1)) {
			var redirectServer = new Object();
			redirectServer.path = "/prodSpecCharValueUse/-";
			redirectServer.op = "add";
			var redCharValueArray  = new Array();
			var redCharValue = new Object();
			redCharValue.value = this.updateRedirect;
			redCharValueArray.push(redCharValue);
			var redCharValueUse = new Object();
			redCharValueUse.name = "redirectServer";
			redCharValueUse.minCardinality = 0;
			redCharValueUse.maxCardinality = 1;
			redCharValueUse.productSpecCharacteristicValue = redCharValueArray;
			var redProductSpec = new Object();
			redProductSpec.id = "8";
			redProductSpec.href = "/catalogManagement/v2/productSpecification/8";
			redCharValueUse.productSpecification = redProductSpec;
			redirectServer.value = redCharValueUse;
			offerNew.push(redirectServer);
		} else if(this.updateRedirect
				&& this.activeItem.prodSpecCharValueUse[redIndex].productSpecCharacteristicValue
				&& this.activeItem.prodSpecCharValueUse[redIndex].productSpecCharacteristicValue[0]
				&& (this.updateRedirect != this.activeItem.prodSpecCharValueUse[redIndex].productSpecCharacteristicValue[0].value)) {
			var redirectServer = new Object();
			redirectServer.path = "/prodSpecCharValueUse/"
					+ redIndex + "/productSpecCharacteristicValue/0/value";
			redirectServer.op = "replace";
			redirectServer.value = this.updateRedirect;
			offerNew.push(redirectServer);
		}
		if(this.activeItem.prodSpecCharValueUse) {
			function sidCheckName(charVal) {
				return charVal.name == "serviceIdentifier";
			}
			var sidIndex = this.activeItem.prodSpecCharValueUse.findIndex(sidCheckName);
		} else {
			var sidIndex = -1;
		}
		if(!this.serviceIdentifier && (sidIndex != -1)) {
			var serviceIdentifier = new Object();
			serviceIdentifier.path = "/prodSpecCharValueUse/" + sidIndex;
			serviceIdentifier.op = "remove";
			offerNew.push(serviceIdentifier);
		} else if(this.serviceIdentifier && (sidIndex == -1)) {
			var serviceIdentifier = new Object();
			serviceIdentifier.path = "/prodSpecCharValueUse/-";
			serviceIdentifier.op = "add";
			var sidCharValueArray  = new Array();
			var sidCharValue = new Object();
			sidCharValue.value = this.serviceIdentifier;
			sidCharValueArray.push(sidCharValue);
			var sidCharValueUse = new Object();
			sidCharValueUse.name = "serviceIdentifier";
			sidCharValueUse.minCardinality = 0;
			sidCharValueUse.maxCardinality = 1;
			sidCharValueUse.productSpecCharacteristicValue = sidCharValueArray;
			var sidProductSpec = new Object();
			sidProductSpec.id = "1";
			sidProductSpec.href = "/catalogManagement/v2/productSpecification/1";
			sidCharValueUse.productSpecification = sidProductSpec;
			serviceIdentifier.value = sidCharValueUse;
			offerNew.push(serviceIdentifier);
		} else if(this.serviceIdentifier
				&& this.activeItem.prodSpecCharValueUse[sidIndex].productSpecCharacteristicValue
				&& this.activeItem.prodSpecCharValueUse[sidIndex].productSpecCharacteristicValue[0]
				&& (this.serviceIdentifier != this.activeItem.prodSpecCharValueUse[sidIndex].productSpecCharacteristicValue[0].value)) {
			var serviceIdentifier = new Object();
			serviceIdentifier.path = "/prodSpecCharValueUse/"
					+ sidIndex + "/productSpecCharacteristicValue/0/value";
			serviceIdentifier.op = "replace";
			serviceIdentifier.value = this.serviceIdentifier;
			offerNew.push(serviceIdentifier);
		}
		if(this.activeItem.prodSpecCharValueUse) {
			function checkPolicyTable(charVal) {
				return charVal.name == "policyTable";
			}
			var polIndex = this.activeItem.prodSpecCharValueUse.findIndex(checkPolicyTable);
		} else {
			var polIndex = -1;
		}
		if(!this.priceUpdatePolicy && (polIndex != -1)) {
			var policyTable= new Object();
			policyTable.path = "/prodSpecCharValueUse/" + polIndex;
			policyTable.op = "remove";
			offerNew.push(policyTable);
		} else if(this.priceUpdatePolicy && (polIndex == -1)) {
			var policyTable = new Object();
			policyTable.path = "/prodSpecCharValueUse/-";
			policyTable.op = "add";
			var polCharValueArray  = new Array();
			var polCharValue = new Object();
			polCharValue.value = this.priceUpdatePolicy;
			polCharValueArray.push(polCharValue);
			var polCharValueUse = new Object();
			polCharValueUse.name = "policyTable";
			polCharValueUse.minCardinality = 0;
			polCharValueUse.maxCardinality = 1;
			polCharValueUse.productSpecCharacteristicValue = polCharValueArray;
			var polProductSpec = new Object();
			polProductSpec.id = "5";
			polProductSpec.href = "/catalogManagement/v2/productSpecification/5";
			polCharValueUse.productSpecification = polProductSpec;
			policyTable.value = polCharValueUse;
			offerNew.push(policyTable);
		} else if(this.priceUpdatePolicy
				&& this.activeItem.prodSpecCharValueUse[polIndex].productSpecCharacteristicValue
				&& this.activeItem.prodSpecCharValueUse[polIndex].productSpecCharacteristicValue[0]
				&& (this.priceUpdatePolicy != this.activeItem.prodSpecCharValueUse[polIndex].productSpecCharacteristicValue[0].value)) {
			var policyTable = new Object();
			policyTable.path = "/prodSpecCharValueUse/"
					+ polIndex + "/productSpecCharacteristicValue/0/value";
			policyTable.op = "replace";
			policyTable.value = this.priceUpdatePolicy;
			offerNew.push(policyTable);
		}
		if(offerNew.length > 0) {
			ajax.body = JSON.stringify(offerNew);
			ajax.generateRequest();
		}
	}

	_updateProductOfferResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var listOffer = document.getElementsByClassName("bundleCheck");
		Array.prototype.forEach.call(listOffer, function(ell) {
			if(ell.checked == true) {
				ell.checked = false;
			}
		});
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.cancelDialog();
	}

	_updateProductOfferError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_updateOfferPrice(event) {
		var ajax =  this.$.updateOfferPriceAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName;
		var updatePriceNew = new Array();
		function checkName(price) {
			return price.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		var indexPrices = this.prices.findIndex(checkName);
		if(!this.priceUpdateDescription && this.prices[indexPrices].description) {
			var descr = new Object();
			descr.op = "remove";
			descr.path = "/productOfferingPrice/" + indexPrices + "/description";
			updatePriceNew.push(descr);
		} else if(this.priceUpdateDescription && !this.prices[indexPrices].description) {
			var descr = new Object();
			descr.op = "add";
			descr.path = "/productOfferingPrice/" + indexPrices + "/description";
			descr.value = this.priceUpdateDescription;
			updatePriceNew.push(descr);
		} else if(this.priceUpdateDescription != this.prices[indexPrices].description) {
			var descr = new Object();
			descr.op = "replace";
			descr.path = "/productOfferingPrice/" + indexPrices + "/description";
			descr.value = this.priceUpdateDescription;
			updatePriceNew.push(descr);
		}
		if(!this.updateOfferStartDatePrice && !this.updateOfferEndDatePrice
				&& (this.prices[indexPrices].start || this.prices[indexPrices].end)) {
			var priceValidity = new Object();
			priceValidity.op = "remove";
			priceValidity.path = "/productOfferingPrice/" + indexPrices + "/validFor";
			updatePriceNew.push(priceValidity);
		} else if((this.updateOfferStartDatePrice || this.updateOfferEndDatePrice)
				&& (!this.prices[indexPrices].start && !this.prices[indexPrices].end)) {
			var priceValidity = new Object();
			priceValidity.op = "add";
			priceValidity.path = "/productOfferingPrice/" + indexPrices + "/validFor";
			var pricePeriod= new Object();
			if(this.updateOfferStartDatePrice) {
				pricePeriod.startDateTime = this.updateOfferStartDatePrice;
			}
			if(this.updateOfferEndDatePrice) {
				pricePeriod.endDateTime = this.updateOfferEndDatePrice;
			}
			priceValidity.value = pricePeriod;
			updatePriceNew.push(priceValidity);
		} else {
			if(!this.updateOfferStartDatePrice && this.prices[indexPrices].start) {
				var priceStart = new Object();
				priceStart.op = "remove";
				priceStart.path = "/productOfferingPrice/" + indexPrices + "/validFor/startDateTime";
				updatePriceNew.push(priceStart);
			} else if(this.updateOfferStartDatePrice && !this.prices[indexPrices].start) {
				var priceStart = new Object();
				priceStart.op = "add";
				priceStart.path = "/productOfferingPrice/" + indexPrices + "/validFor/startDateTime";
				priceStart.value = this.updateOfferStartDatePrice;
				updatePriceNew.push(priceStart);
			} else if(this.prices[indexPrices].start
					&& !this.updateOfferStartDatePrice.startsWith(this.prices[indexPrices].start)) {
				var priceStart = new Object();
				priceStart.op = "replace";
				priceStart.path = "/productOfferingPrice/" + indexPrices + "/validFor/startDateTime";
				priceStart.value = this.updateOfferStartDatePrice;
				updatePriceNew.push(priceStart);
			}
			if(!this.updateOfferEndDatePrice && this.prices[indexPrices].end) {
				var priceEnd = new Object();
				priceEnd.op = "remove";
				priceEnd.path = "/productOfferingPrice/" + indexPrices + "/validFor/endDateTime";
				updatePriceNew.push(priceEnd);
			} else if(this.updateOfferEndDatePrice && !this.prices[indexPrices].end) {
				var priceEnd = new Object();
				priceEnd.op = "add";
				priceEnd.path = "/productOfferingPrice/" + indexPrices + "/validFor/endDateTime";
				priceEnd.value = this.updateOfferEndDatePrice;
				updatePriceNew.push(priceEnd);
			} else if(this.prices[indexPrices].end
					&& !this.updateOfferEndDatePrice.startsWith(this.prices[indexPrices].end)) {
				var priceEnd = new Object();
				priceEnd.op = "replace";
				priceEnd.path = "/productOfferingPrice/" + indexPrices + "/validFor/endDateTime";
				priceEnd.value = this.updateOfferEndDatePrice;
				updatePriceNew.push(priceEnd);
			}
		}
		if(!this.priceUpdateType && this.prices[indexPrices].priceType) {
			var priceType = new Object();
			priceType.op = "remove";
			priceType.path = "/productOfferingPrice/" + indexPrices + "/priceType";
			updatePriceNew.push(priceType);
		} else if(this.priceUpdateType) {
			var priceType = new Object();
			priceType.path = "/productOfferingPrice/" + indexPrices + "/priceType";
			switch(this.priceUpdateType) {
				case "Recurring":
					priceType.value = "recurring";
					break;
				case "One Time":
					priceType.value = "one_time";
					break;
				case "Usage":
					priceType.value = "usage";
					break;
				case "Tariff":
					priceType.value = "tariff";
					break;
			}
			if(!this.prices[indexPrices].priceType) {
				priceType.op = "add";
				updatePriceNew.push(priceType);
			} else if(this.priceUpdateType !== this.prices[indexPrices].priceType) {
				priceType.op = "replace";
				updatePriceNew.push(priceType);
			}
		}
		if(!this.priceUpdatePla && this.prices[indexPrices].pla) {
			var pricePla = new Object();
			pricePla.op = "remove";
			pricePla.path = "/productOfferingPrice/" + indexPrices + "/pricingLogicAlgorithm";
			updatePriceNew.push(pricePla);
		} else if(this.priceUpdatePla) {
			if(!this.prices[indexPrices].pla) {
				var pricePla = new Object();
				pricePla.path = "/productOfferingPrice/" + indexPrices + "/pricingLogicAlgorithm";
				pricePla.op = "add";
				pricePla.value = new Array();
				var plaRef = new Object();
				plaRef.href = this.priceUpdatePla;
				pricePla.value.push(plaRef);
				updatePriceNew.push(pricePla);
			} else if(this.priceUpdatePla !== this.prices[indexPrices].pla) {
				var pricePla = new Object();
				pricePla.path = "/productOfferingPrice/" + indexPrices + "/pricingLogicAlgorithm/0/href";
				pricePla.op = "replace";
				pricePla.value = this.priceUpdatePla;
				updatePriceNew.push(pricePla);
			}
		}
		if(!this.priceUpdateSize && this.prices[indexPrices].size) {
			var unitOfMeasure = new Object();
			unitOfMeasure.op = "remove";
			unitOfMeasure.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
			updatePriceNew.push(unitOfMeasure);
		} else if(this.priceUpdateSize && this.priceUpdateUnits) {
			if(this.priceUpdateUnits == "Seconds") {
				var priceSizeLast = this.priceUpdateSize.slice(-1);
				var priceSizeN = new Number();
				if(priceSizeLast === "s") {
					priceSizeN = parseInt(this.priceUpdateSize);
				} else if(priceSizeLast === "m") {
					priceSizeN = parseInt(this.priceUpdateSize) * 60;
				} else if(priceSizeLast === "h") {
					priceSizeN = parseInt(this.priceUpdateSize) * 3600;
				} else {
					priceSizeN = parseInt(this.priceUpdateSize);
				}
				if((this.prices[indexPrices].unit != "s")
						 || (this.prices[indexPrices].size != priceSizeN)) {
					var unitOfMeasure = new Object();
					unitOfMeasure.op = "replace";
					unitOfMeasure.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
					unitOfMeasure.value = priceSizeN.toString() + "s";
					updatePriceNew.push(unitOfMeasure);
				}
			} else if(this.priceUpdateUnits == "Bytes") {
				var priceSizeLast = this.priceUpdateSize.slice(-1);
				var priceSizeN = new Number();
				if(priceSizeLast === "b") {
					priceSizeN = parseInt(this.priceUpdateSize);
				} else if(priceSizeLast === "k") {
					priceSizeN = parseInt(this.priceUpdateSize) * 1000;
				} else if(priceSizeLast === "m") {
					priceSizeN = parseInt(this.priceUpdateSize) * 1000000;
				} else if(priceSizeLast === "g") {
					priceSizeN = parseInt(this.priceUpdateSize) * 1000000000;
				} else {
					priceSizeN = parseInt(this.priceUpdateSize);
				}
				if((this.prices[indexPrices].unit != "b")
						 || (this.prices[indexPrices].size != priceSizeN)) {
					var unitOfMeasure = new Object();
					unitOfMeasure.op = "replace";
					unitOfMeasure.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
					unitOfMeasure.value = priceSizeN.toString() + "b";
					updatePriceNew.push(unitOfMeasure);
				}
			} else if(this.priceUpdateUnits == "Messages") {
				var priceSizeLast = this.priceUpdateSize.slice(-1);
				var priceSizeN = new Number();
				if(priceSizeLast === "msg") {
					priceSizeN = parseInt(this.priceUpdateSize);
				} else {
					priceSizeN = parseInt(this.priceUpdateSize);
				}
				if((this.prices[indexPrices].unit != "msg")
						|| (this.prices[indexPrices].size != priceSizeN)) {
					var unitOfMeasure = new Object();
					unitOfMeasure.op = "replace";
					unitOfMeasure.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
					unitOfMeasure.value = priceSizeN.toString() + "msg";
					updatePriceNew.push(unitOfMeasure);
				}
			}
		}
		if(!this.priceUpdateAmount && this.prices[indexPrices].amount) {
			var priceAmount = new Object();
			priceAmount.op = "remove";
			priceAmount.path = "/productOfferingPrice/" + indexPrices + "/price";
			updatePriceNew.push(priceAmount);
		} else if(this.priceUpdateAmount && !this.prices[indexPrices].amount) {
			var priceAmount = new Object();
			priceAmount.op = "add";
			priceAmount.path = "/productOfferingPrice/" + indexPrices + "/price";
			priceAmount.value = {"taxIncludedAmount": this.priceUpdateAmount};
			updatePriceNew.push(priceAmount);
		} else if(this.priceUpdateAmount !== this.prices[indexPrices].amount) {
			var priceAmount = new Object();
			priceAmount.op = "replace";
			priceAmount.path = "/productOfferingPrice/" + indexPrices + "/price";
			priceAmount.value = {"taxIncludedAmount": this.priceUpdateAmount};
			updatePriceNew.push(priceAmount);
		}
		if(!this.priceUpdatePeriod && this.prices[indexPrices].period) {
			var pricePeriod = new Object();
			priceperiod.op = "remove";
			pricePeriod.path = "/productOfferingPrice/" + indexPrices + "/recurringChargePeriod";
			updatePriceNew.push(pricePeriod);
		} else if(this.priceUpdatePeriod) {
			var pricePeriod = new Object();
			priceCharge.path = "/productOfferingPrice/" + indexPrices + "/recurringChargePeriod";
			switch(this.priceUpdatePeriod) {
				case "Hourly":
					pricePeriod.value = "hourly";
					break;
				case "Daily":
					pricePeriod.value = "daily";
					break;
				case "Weekly":
					pricePeriod.value = "weekly";
					break;
				case "Monthly":
					pricePeriod.value = "monthly";
					break;
				case "Yearly":
					pricePeriod.value = "yearly";
					break;
			}
			if(!this.prices[indexPrices].period) {
				priceperiod.op = "add";
				updatePriceNew.push(pricePeriod);
			} else if(this.priceUpdatePeriod != this.prices[indexPrices].period) {
				priceCharge.op = "replace";
				updatePriceNew.push(pricePeriod);
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function rrtCheckName(charVal) {
				return charVal.name == "radiusReserveTime";
			}
			var rrtIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(rrtCheckName);
		} else {
			var rrtIndex = -1;
		}
		if(!this.priceUpdateReserveTime && (rrtIndex != -1)) {
			var radiusReserveTime = new Object();
			radiusReserveTime.op = "remove";
			radiusReserveTime.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + rrtIndex;
			updatePriceNew.push(radiusReserveTime);
		} else if(this.priceUpdateReserveTime) {
			if(rrtIndex == -1) {
				var radiusReserveTime = new Object();
				radiusReserveTime.op = "add";
				radiusReserveTime.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var rrtCharValueUse = new Object();
				rrtCharValueUse.name = "radiusReserveTime";
				rrtCharValueUse.minCardinality = 0;
				rrtCharValueUse.maxCardinality = 1;
				var rrtCharValueArray  = new Array();
				var rrtCharValue = this.timeCharValue(this.priceUpdateReserveTime);
				rrtCharValue.default = true;
				rrtCharValueArray.push(rrtCharValue);
				rrtCharValueUse.productSpecCharacteristicValue = rrtCharValueArray;
				var rrtProductSpec = new Object();
				rrtProductSpec.id = "1";
				rrtProductSpec.href = "/catalogManagement/v2/productSpecification/1";
				rrtCharValueUse.productSpecification = rrtProductSpec;
				radiusReserveTime.value = rrtCharValueUse;
				updatePriceNew.push(radiusReserveTime);
			} else {
				var rrtCharValue = this.timeCharValue(this.priceUpdateReserveTime);
				if((rrtCharValue.value != this.prices[indexPrices].productSpecCharacteristicValue[0].value)
						|| (rrtCharValue.unitOfMeasure != this.prices[indexPrices].productSpecCharacteristicValue[0].unitOfMeasure)) {
					var radiusReserveTime = new Object();
					radiusReserveTime.op = "replace";
					radiusReserveTime.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
							+ rrtIndex + "/productSpecCharacteristicValue/0";
					radiusReserveTime.op = "replace";
					radiusReserveTime.value = rrtCharValue;
					updatePriceNew.push(radiusReserveTime);
				}
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function rroCheckName(charVal) {
				return charVal.name == "radiusReserveOctets";
			}
			var rroIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(rroCheckName);
		} else {
			var rroIndex = -1;
		}
		if(!this.priceUpdateReserveOctets && (rroIndex != -1)) {
			var radiusReserveOctets = new Object();
			radiusReserveOctets.op = "remove";
			radiusReserveOctets.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + rroIndex;
			updatePriceNew.push(radiusReserveOctets);
		} else if(this.priceUpdateReserveOctets) {
			if(rroIndex == -1) {
				var radiusReserveOctets = new Object();
				radiusReserveOctets.op = "add";
				radiusReserveOctets.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var rroCharValueUse = new Object();
				rroCharValueUse.name = "radiusReserveOctets";
				rroCharValueUse.minCardinality = 0;
				rroCharValueUse.maxCardinality = 1;
				var rroCharValueArray  = new Array();
				var rroCharValue = this.octetsCharValue(this.priceUpdateReserveOctets);
				rroCharValue.default = true;
				rroCharValueArray.push(rroCharValue);
				rroCharValueUse.productSpecCharacteristicValue = rroCharValueArray;
				var rroProductSpec = new Object();
				rroProductSpec.id = "1";
				rroProductSpec.href = "/catalogManagement/v2/productSpecification/1";
				rroCharValueUse.productSpecification = rroProductSpec;
				radiusReserveOctets.value = rroCharValueUse;
				updatePriceNew.push(radiusReserveOctets);
			} else {
				var rroCharValue = this.octetsCharValue(this.priceUpdateReserveOctets);
				if((rroCharValue.value != this.prices[indexPrices].prodSpecCharValueUse[rroIndex].value)
						|| (rroCharValue.unitOfMeasure != this.prices[indexPrices].prodSpecCharValueUse[rroIndex].unitOfMeasure)) {
					var radiusReserveOctets = new Object();
					radiusReserveOctets.op = "replace";
					radiusReserveOctets.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
							+ rroIndex + "/productSpecCharacteristicValue/0";
					radiusReserveOctets.op = "replace";
					radiusReserveOctets.value = rroCharValue;
					updatePriceNew.push(radiusReserveOctets);
				}
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function tarCheckName(charVal) {
				return charVal.name == "destPrefixTariffTable";
			}
			var tarIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(tarCheckName);
		} else {
			var tarIndex = -1;
		}
		if(!this.priceUpdateTariff && (tarIndex != -1)) {
			var tariffTable = new Object();
			tariffTable.op = "remove";
			tariffTable.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + tarIndex;
			updatePriceNew.push(tariffTable);
		} else if(this.priceUpdateTariff) {
			if(tarIndex == -1) {
				var tariffTable = new Object();
				tariffTable.op = "add";
				tariffTable.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var tarCharValueUse = new Object();
				tarCharValueUse.name = "destPrefixTariffTable";
				tarCharValueUse.minCardinality = 0;
				tarCharValueUse.maxCardinality = 1;
				var tarCharValueArray  = new Array();
				var tarCharValue = new Object();
				tarCharValue.default = true;
				tarCharValue.value = this.priceUpdateTariff;
				tarCharValueArray.push(tarCharValue);
				tarCharValueUse.productSpecCharacteristicValue = tarCharValueArray;
				var tarProductSpec = new Object();
				tarProductSpec.id = "5";
				tarProductSpec.href = "/catalogManagement/v2/productSpecification/5";
				tarCharValueUse.productSpecification = tarProductSpec;
				tariffTable.value = tarCharValueUse;
				updatePriceNew.push(tariffTable);
			} else if(this.priceUpdateTariff != this.prices[indexPrices].prodSpecCharValueUse[tarIndex].value) {
				var tariffTable = new Object();
				tariffTable.op = "replace";
				tariffTable.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
						+ tarIndex + "/productSpecCharacteristicValue/0/value";
				tariffTable.value = this.priceUpdateTariff;
				updatePriceNew.push(tariffTable);
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function checkChargingKey(charge) {
				return charge.name == "chargingKey";
			}
			var ckeyIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChargingKey);
		} else {
			var ckeyIndex = -1;
		}
		if(!this.priceUpdateChargingKey && (ckeyIndex != -1)) {
			var chargingKey = new Object();
			chargingKey.op = "remove";
			chargingKey.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + ckeyIndex;
			updatePriceNew.push(chargingKey);
		} else if(this.priceUpdateChargingKey) {
			if(ckeyIndex == -1) {
				var chargingKey = new Object();
				chargingKey.op = "add";
				chargingKey.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var ckeyCharValueUse = new Object();
				ckeyCharValueUse.name = "chargingKey";
				ckeyCharValueUse.minCardinality = 0;
				ckeyCharValueUse.maxCardinality = 1;
				var ckeyCharValueArray  = new Array();
				var ckeyCharValue = new Object();
				ckeyCharValue.default = true;
				ckeyCharValue.value = this.priceUpdateChargingKey;
				ckeyCharValueArray.push(ckeyCharValue);
				ckeyCharValueUse.productSpecCharacteristicValue = ckeyCharValueArray;
				var ckeyProductSpec = new Object();
				ckeyProductSpec.id = "3";
				ckeyProductSpec.href = "/catalogManagement/v2/productSpecification/3";
				ckeyCharValueUse.productSpecification = ckeyProductSpec;
				chargingKey.value = ckeyCharValueUse;
				updatePriceNew.push(chargingKey);
			} else if(this.priceUpdateChargingKey != this.prices[indexPrices].prodSpecCharValueUse[ckeyIndex].value) {
				var chargingKey = new Object();
				chargingKey.op = "replace";
				chargingKey.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
						+ ckeyIndex + "/productSpecCharacteristicValue/0/value";
				chargingKey.value = this.priceUpdateChargingKey;
				updatePriceNew.push(chargingKey);
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function checkRoamingTable(charVal) {
				return charVal.name == "roamingTable";
			}
			var roamIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkRoamingTable);
		} else {
			var roamIndex = -1;
		}
		if(!this.priceUpdateRoamingTable && (roamIndex != -1)) {
			var roamingTable = new Object();
			roamingTable.op = "remove";
			roamingTable.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + roamIndex;
			updatePriceNew.push(roamingTable);
		} else if(this.priceUpdateRoamingTable) {
			if(roamIndex == -1) {
				var roamingTable = new Object();
				roamingTable.op = "add";
				roamingTable.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var roamCharValueUse = new Object();
				roamCharValueUse.name = "roamingTable";
				roamCharValueUse.minCardinality = 0;
				roamCharValueUse.maxCardinality = 1;
				var roamCharValueArray  = new Array();
				var roamCharValue = new Object();
				roamCharValue.default = true;
				roamCharValue.value = this.priceUpdateRoamingTable;
				roamCharValueArray.push(roamCharValue);
				roamCharValueUse.productSpecCharacteristicValue = roamCharValueArray;
				var roamProductSpec = new Object();
				roamProductSpec.id = "4";
				roamProductSpec.href = "/catalogManagement/v2/productSpecification/4";
				roamCharValueUse.productSpecification = roamProductSpec;
				roamingTable.value = roamCharValueUse;
				updatePriceNew.push(roamingTable);
			} else if(this.priceUpdateRoamingTable!= this.prices[indexPrices].prodSpecCharValueUse[roamIndex].value) {
				var roamingTable = new Object();
				roamingTable.op = "replace";
				roamingTable.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
						+ roamIndex + "/productSpecCharacteristicValue/0/value";
				roamingTable.value = this.priceUpdateRoamingTable;
				updatePriceNew.push(roamingTable);
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function checkCallDirection(charVal) {
				return charVal.name == "callDirection";
			}
			var cdirIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCallDirection);
		} else {
			var cdirIndex = -1;
		}
		if(((!this.priceUpdateCallDirIn && !this.priceUpdateCallDirOut)
				|| (this.priceUpdateCallDirIn && this.priceUpdateCallDirOut))
				&& (cdirIndex != -1)) {
			var callDirection = new Object();
			callDirection.op = "remove";
			callDirection.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + cdirIndex;
			updatePriceNew.push(callDirection);
		} else if((this.priceUpdateCallDirIn || this.priceUpdateCallDirOut)
				&& !(this.priceUpdateCallDirIn && this.priceUpdateCallDirOut)) {
			if(cdirIndex == -1) {
				var callDirection = new Object();
				callDirection.op = "add";
				callDirection.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var cdirCharValueUse = new Object();
				cdirCharValueUse.name = "callDirection";
				cdirCharValueUse.minCardinality = 0;
				cdirCharValueUse.maxCardinality = 1;
				var cdirCharValueArray  = new Array();
				var cdirCharValue = new Object();
				cdirCharValue.default = true;
				if(this.priceUpdateCallDirIn) {
					cdirCharValue.value = "answer";
				} else if(this.priceUpdateCallDirOut) {
					cdirCharValue.value = "originate";
				}
				cdirCharValueArray.push(cdirCharValue);
				cdirCharValueUse.productSpecCharacteristicValue = cdirCharValueArray;
				var cdirProductSpec = new Object();
				cdirProductSpec.id = "4";
				cdirProductSpec.href = "/catalogManagement/v2/productSpecification/4";
				cdirCharValueUse.productSpecification = cdirProductSpec;
				callDirection.value = cdirCharValueUse;
				updatePriceNew.push(callDirection);
			} else {
				var callDirection = new Object();
				callDirection.op = "replace";
				callDirection.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
						+ cdirIndex + "/productSpecCharacteristicValue/0/value";
				if(this.priceUpdateCallDirIn) {
					callDirection.value = "answer";
				} else if(this.priceUpdateCallDirOut) {
					callDirection.value = "originate";
				}
				updatePriceNew.push(callDirection);
			}
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function checkCharTime(charVal) {
				return charVal.name == "timeOfDayRange";
			}
			var todIndex = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCharTime);
		} else {
			var todIndex = -1;
		}
		if(!this.priceUpdateTODStart && !this.priceUpdateTODEnd && (todIndex != -1)) {
			var timeOfDay = new Object();
			timeOfDay.op = "remove";
			timeOfDay.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + todIndex;
			updatePriceNew.push(timeOfDay);
		} else if(this.priceUpdateTODStart && this.priceUpdateTODEnd) {
			if(todIndex == -1) {
				var timeOfDay = new Object();
				timeOfDay.op = "add";
				timeOfDay.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/-";
				var todCharValueUse = new Object();
				todCharValueUse.name = "timeOfDayRange";
				todCharValueUse.valueType = "Range";
				todCharValueUse.minCardinality = 0;
				todCharValueUse.maxCardinality = 1;
				var todCharValueArray  = new Array();
				var todCharValue = new Object();
				var todRange = new Object();
				var todRangeLower = new Object();
				var todEpochStart = new Date("1970-01-01T" + this.priceUpdateTODStart);
				todRangeLower.amount = (todEpochStart.getHours() * 60) + todEpochStart.getMinutes();
				todRangeLower.units = "minutes";
				todRange.lowerValue = todRangeLower;
				var todRangeUpper = new Object();
				var todEpochEnd = new Date("1970-01-01T" + this.priceUpdateTODEnd);
				todRangeUpper.amount = (todEpochEnd.getHours() * 60) + todEpochEnd.getMinutes();
				todRangeUpper.units = "minutes";
				todRange.upperValue = todRangeUpper;
				todCharValue.value = todRange;
				todCharValueArray.push(todCharValue);
				todCharValueUse.productSpecCharacteristicValue = todCharValueArray;
				var todProductSpec = new Object();
				todProductSpec.id = "3";
				todProductSpec.href = "/catalogManagement/v2/productSpecification/3";
				todCharValueUse.productSpecification = todProductSpec;
				timeOfDay.value = todCharValueUse;
				updatePriceNew.push(timeOfDay);
			} else {
				if(this.priceUpdateTODStart != this.prices[indexPrices].prodSpecCharValueUse[todIndex].value
						|| this.priceUpdateTODEnd != this.prices[indexPrices].prodSpecCharValueUse[todIndex].value) {
					var timeOfDayLower = new Object();
					timeOfDayLower.op = "replace";
					timeOfDayLower.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
							+ todIndex + "/productSpecCharacteristicValue/0/value/lowerValue/amount";
					var todEpochStart = new Date("1970-01-01T" + this.priceUpdateTODStart);
					timeOfDayLower.value = (todEpochStart.getHours() * 60) + todEpochStart.getMinutes();
					updatePriceNew.push(timeOfDayLower);
					var timeOfDayUpper = new Object();
					timeOfDayUpper.op = "replace";
					timeOfDayUpper.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/"
							+ todIndex + "/productSpecCharacteristicValue/0/value/upperValue/amount";
					var todEpochEnd = new Date("1970-01-01T" + this.priceUpdateTODEnd);
					timeOfDayLower.value = (todEpochEnd.getHours() * 60) + todEpochEnd.getMinutes();
					updatePriceNew.push(timeOfDayUpper);
				}
			}
		}
		if(updatePriceNew.length > 0) {
			ajax.body = JSON.stringify(updatePriceNew);
			ajax.generateRequest();
		}
	}

	_updateOfferPriceResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.cancelDialog();
	}

	_updateOfferPriceError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_updateOfferAlteration(event) {
		var ajax =  this.$.updateOfferAlterationAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName;
		var updateAlterationNew = new Array();
		function checkAlterationName(alts) {
			return alts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;;
		}
		var indexAlt = this.alterations.findIndex(checkAlterationName);
		if(this.$.updateAltName.value != this.alterations[indexAlt].name) {
			var alterationName = new Object();
			alterationName.op = "add";
			alterationName.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/name";
			alterationName.value = this.$.updateAltName.value;
			updateAlterationNew.push(alterationName);
		}
		if(this.altUpdateDescription != this.alterations[indexAlt].description) {
			var alterationDesc = new Object();
			alterationDesc.op = "add";
			alterationDesc.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/description";
			alterationDesc.value = this.altUpdateDescription;
			if(alterationDesc.value == "") {
				var alterationDescDel = new Object();
				alterationDescDel.op = "remove";
				alterationDescDel.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/description";
				updateAlterationNew.push(alterationDescDel);
			} else {
				updateAlterationNew.push(alterationDesc);
			}
		}
		if(this.altUpdateType != this.alterations[indexAlt].priceType) {
			var alterationType = new Object();
			alterationType.op = "add";
			alterationType.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/priceType";
			switch(this.altUpdateType) {
				case "Recurring":
					alterationType.value = "recurring";
					break;
				case "One Time":
					alterationType.value = "one_time";
					break;
				case "Usage":
					alterationType.value = "usage";
					break;
			}
			if(alterationType.value == "") {
				var alterationTypeDel = new Object();
				alterationTypeDel.op = "remove";
				alterationTypeDel.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/priceType";
				updateAlterationNew.push(alterationTypeDel);
			} else {
				updateAlterationNew.push(alterationType);
			}
		}
		if(this.$.updateAltSize.value != this.alterations[indexAlt].size) {
			var alterationSize = new Object();
			alterationSize.op = "add";
			alterationSize.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/unitOfMeasure";
			for(var indexUnit1 in this.alterations) {
				if(this.$.updateAltsUnitsdrop.value == "Seconds") {
					this.alterations[indexUnit1].unit = "s";
				}
				if(this.$.updateAltsUnitsdrop.value == "Bytes") {
					this.alterations[indexUnit1].unit = "b";
				}
				var unitDrop = this.alterations[indexUnit1].unit;
				var sizeVal = this.$.updateAltSize.value + unitDrop;
				if(unitDrop && sizeVal) {
					var m = sizeVal.slice(-1);
					if(isNaN(parseInt(m))) {
						var s = sizeVal.slice(0, -1);
					} else {
						var s = sizeVal.size;
					}
					if(unitDrop == "b") {
						if (m == "m") {
							alterationSize.value = s + "000000b";
						} else if(m == "g") {
							alterationSize.value = s + "000000000b";
						} else if(m == "k") {
							alterationSize.value = s + "000b";
						} else {
							alterationSize.value = s + "b";
						}
					} else if(unitDrop == "s") {
						var n = Number(s);
						if(m == "m") {
							n = n * 60;
							alterationSize.value = n.toString() + "s";
						} else if(m == "h") {
							n = n * 3600;
							alterationSize.value = n.toString() + "s";
						} else {
							alterationSize.value = n.toString() + "s";
						}
					}
				}
				if(alterationSize.value == "") {
					var alterationSizeDel = new Object();
					alterationSizeDel.op = "remove";
					alterationSizeDel.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/unitOfMeasure";
					updateAlterationNew.push(alterationSizeDel);
				} else {
					updateAlterationNew.push(alterationSize);
				}
			}
		}
		if(this.$.updateAltAmount.value != this.alterations[indexAlt].amount) {
			var altAmount = new Object();
			altAmount.op = "add";
			altAmount.path = "/productOfferingPrice/" + indexAlt + "/price/taxIncludedAmount";
			altAmount.value = this.$.updateAltAmount.value;
			if(altAmount.value == "") {
				var altAmountDel = new Object();
				altAmountDel.op = "remove";
				altAmountDel.path = "/productOfferingPrice/" + indexAlt + "/price/taxIncludedAmount";
				updateAlterationNew.push(altAmountDel);
			} else {
				updateAlterationNew.push(altAmount);
			}
		}
		if(this.$.addAltPeriodDrop.value != this.alterations[indexAlt].period && !this.$.addAltPeriodDrop.disabled) {
			var altCharge = new Object();
			altCharge.op = "add";
			altCharge.path = "/productOfferingPrice/" + indexAlt + "/recurringChargePeriod";
			switch(this.$.updateAltPeriod.selected) {
				case 0:
					altCharge.value = "hourly";
					break;
				case 1:
					altCharge.value = "daily";
					break;
				case 2:
					altCharge.value = "weekly";
					break;
				case 3:
					altCharge.value = "monthly";
					break;
				case 4:
					altCharge.value = "yearly";
			}
			if(altCharge.value == "") {
				var altChargeDel = new Object();
				altChargeDel.op = "remove";
				altChargeDel.path = "/productOfferingPrice/" + indexAlt + "/recurringChargePeriod";
				updateAlterationNew.push(altChargeDel);
			} else {
				updateAlterationNew.push(altCharge);
			}
		}
		ajax.body = JSON.stringify(updateAlterationNew);
		ajax.generateRequest();
	}

	_updateOfferAlterationResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.cancelDialog();
	}

	_updateOfferAlterationError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	checkPattern() {
		if(this.$.updatePriceUnits.selected == 0) {
			this.$.updatePriceSize.allowedPattern = "[0-9kmg]";
			this.$.updatePriceSize.pattern = "^[0-9]+[kmg]?$";
			this.$.updatePriceSize.disabled = false;
			this.$.priceUpdateReserveTimeInput.disabled = true;
			this.$.priceUpdateReserveOctetsInput.disabled = false;
		} else if(this.$.updatePriceUnits.selected == 1) {
			this.$.updatePriceSize.allowedPattern = "[0-9]";
			this.$.updatePriceSize.pattern = "^[0-9]+$";
			this.$.updatePriceSize.disabled = true;
			this.$.priceUpdateReserveTimeInput.disabled = true;
			this.$.priceUpdateReserveOctetsInput.disabled = true;
		} else if(this.$.updatePriceUnits.selected == 2) {
			this.$.updatePriceSize.allowedPattern = "[0-9mh]";
			this.$.updatePriceSize.pattern = "^[0-9]+[mh]?$";
			this.$.updatePriceSize.disabled = false;
			this.$.priceUpdateReserveTimeInput.disabled = false;
			this.$.priceUpdateReserveOctetsInput.disabled = true;
		}
	}

	checkPatternAlt() {
		if(this.$.updateAlterationUnits.selected == 0) {
			this.$.updateAltSize.allowedPattern = "[0-9kmg]";
			this.$.updateAltSize.pattern = "^[0-9]+[kmg]?$";
		} else if(this.$.updateAlterationUnits.selected == 1) {
			this.$.updateAltSize.allowedPattern = "[0-9]";
			this.$.updateAltSize.pattern = "^[0-9]+$";
		} else if(this.$.updateAlterationUnits.selected == 2) {
			this.$.updateAltSize.allowedPattern = "[0-9mh]";
			this.$.updateAltSize.pattern = "^[0-9]+[mh]?$";
		}
	}

	checkRecurring() {
		if(this.$.updatePriceType.selected == 0) {
			this.$.updatePricePerioddrop.disabled = false;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.priceUpdateReserveTimeInput.disabled = true;
			this.$.priceUpdateReserveOctetsInput.disabled = true;
			this.$.updatePriceUnits.selected = 1;
			this.$.updatePriceAmount.disabled = false;
			this.$.updatePla.disabled = true;
		} else if(this.$.updatePriceType.selected == 1) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.priceUpdateReserveTimeInput.disabled = true;
			this.$.priceUpdateReserveOctetsInput.disabled = true;
			this.$.updatePriceUnits.selected = 1;
			this.$.updatePriceAmount.disabled = false;
			this.$.updatePla.disabled = true;
		} else if(this.$.updatePriceType.selected == 2) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			if(this.updateOfferSpecification == "PrepaidData") {
				this.$.priceBytes.disabled = false;
				this.$.priceSeconds.disabled = false;
				this.$.updatePriceUnits.selected = 0;
			}
			if(this.updateOfferSpecification == "PrepaidVoice") {
				this.$.priceSeconds.disabled = false;
				this.$.priceBytes.disabled = true;
				this.$.updatePriceUnits.selected = 2;
			}
			this.$.updatePriceAmount.disabled = false;
			this.$.updatePla.disabled = true;
		} else if(this.$.updatePriceType.selected == 3) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			if(this.updateOfferSpecification == "PrepaidData") {
				this.$.priceBytes.disabled = false;
				this.$.priceSeconds.disabled = false;
				this.$.updatePriceUnits.selected = 0;
			}
			if(this.updateOfferSpecification == "PrepaidVoice") {
				this.$.priceSeconds.disabled = false;
				this.$.priceBytes.disabled = true;
				this.$.updatePriceUnits.selected = 2;
			}
			this.$.updatePriceAmount.disabled = true;
			this.$.updatePriceAmount.value = null;
			this.$.updatePla.disabled = false;
		}
	}

	checkRecurringAlt() {
		if(this.$.updateAltType.selected == 0) {
			this.$.addAltPeriodDrop.disabled = false;
			if(this.updateOfferSpecification == "PrepaidData") {
				this.$.altBytes.disabled = false;
				this.$.altSeconds.disabled = false;
			}
			if(this.updateOfferSpecification == "PrepaidVoice") {
				this.$.altSeconds.disabled = false;
				this.$.altBytes.disabled = true;
			}
			this.$.altCents.disabled = false;
			this.$.updateAlterationUnits.selected = 1;
		} else if(this.$.updateAltType.selected == 1) {
			this.$.addAltPeriodDrop.disabled = true;
			if(this.updateOfferSpecification == "PrepaidData") {
				this.$.altBytes.disabled = false;
				this.$.altSeconds.disabled = false;
			}
			if(this.updateOfferSpecification == "PrepaidVoice") {
				this.$.altSeconds.disabled = false;
				this.$.altBytes.disabled = true;
			}
			this.$.altCents.disabled = false;
			this.$.updateAlterationUnits.selected = 1;
		} else if(this.$.updateAltType.selected == 2) {
			this.$.addAltPeriodDrop.disabled = true;
			if(this.updateOfferSpecification == "PrepaidData") {
				this.$.altBytes.disabled = false;
				this.$.altSeconds.disabled = false;
				this.$.updateAlterationUnits.selected = 0;
			}
			if(this.updateOfferSpecification == "PrepaidVoice") {
				this.$.altSeconds.disabled = false;
				this.$.altBytes.disabled = true;
				this.$.updateAlterationUnits.selected = 2;
			}
			this.$.altCents.disabled = true;
		}
	}

	_updateAddPrice(event) {
		function priceUpdateCallDirPriceName(updatePrice) {
			return updatePrice.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		var updateIndexPrice = this.prices.findIndex(priceUpdateCallDirPriceName);
		if(updateIndexPrice == -1) {
			var updatePriceNew = new Object();
		} else {
			var updatePriceNew = this.prices[updateIndexPrice];
		}
		updatePriceNew.name = this.$.updatePriceName.value;
		updatePriceNew.description = this.priceUpdateDescription;
		updatePriceNew.start = this.updateOfferStartDatePrice;
		updatePriceNew.end = this.updateOfferEndDatePrice;
		switch(this.$.updatePriceType.selected) {
			case 0:
				updatePriceNew.priceType = "recurring";
				break;
			case 1:
				updatePriceNew.priceType = "one_time";
				break;
			case 2:
				updatePriceNew.priceType = "usage";
				break;
			case 3:
				updatePriceNew.priceType = "tariff";
				break;
		}
		updatePriceNew.pla = this.updatePricePla;
		switch(this.$.updatePriceUnits.selected) {
			case 0:
				updatePriceNew.unit = "b";
				break;
			case 1:
				updatePriceNew.unit = "c";
				break;
			case 2:
				updatePriceNew.unit = "s";
				break;
			case 3:
				updatePriceNew.unit = "msg";
				break;
		}
		updatePriceNew.amount = this.priceUpdateAmount;
		updatePriceNew.size = this.priceUpdateSize;
		updatePriceNew.currency = this.priceUpdateCurrency;
		switch(this.$.updatePricePeriod.selected) {
			case 0:
				updatePriceNew.period = "hourly";
				break;
			case 1:
				updatePriceNew.period = "daily";
				break;
			case 2:
				updatePriceNew.period = "weekly";
				break;
			case 3:
				updatePriceNew.period = "monthly";
				break
			case 4:
				updatePriceNew.period = "yearly";
				break;
		}
		if(this.priceUpdateAlteration) {
			function checkAlt(alts) {
				return alts.name == this.priceUpdateAlteration;
			}
			updatePriceNew.alteration = this.alterations.findIndex(checkAlt);
		}
		if(updatePriceNew.name
					&& (updatePriceNew.amount || updatePriceNew.priceType == "tariff")
					&& updatePriceNew.priceType
					&& updatePriceNew.unit) {
			if(updateIndexPrice == -1) {
				this.push('prices', updatePriceNew);
			}
			var ajax =  this.$.updateOfferPriceAjax;
			ajax.method = "PATCH";
			ajax.contentType = "application/json-patch+json";
			ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName;
			var updatePriceNew1 = new Array();
			function checkName(price) {
				return price.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
			}
			var indexPrices = this.prices.findIndex(checkName);
			var createPrice = new Object();
			createPrice.op = "add";
			createPrice.path = "/productOfferingPrice/-";
			var addValue = new Object();
			if(this.$.updatePriceName.value) {
				addValue.name = this.$.updatePriceName.value;
			}
			if(this.priceUpdateDescription) {
				addValue.description = this.priceUpdateDescription;
			}
			var startDateTime;
			var endDateTime;
			if(this.updateOfferStartDatePrice) {
				startDateTime = this.updateOfferStartDatePrice;
			}
			if(this.updateOfferEndDatePrice) {
				endDateTime = this.updateOfferEndDatePrice;
			}
			if(startDateTime && endDateTime) {
				addValue.validFor = {startDateTime, endDateTime};
			} else if(startDateTime && !endDateTime) {
				addValue.validFor = {startDateTime};
			} else if(!startDateTime && endDateTime) {
				addValue.validFor = {endDateTime};
			}
			if(this.priceUpdateType) {
				switch(this.$.updatePriceType.selected) {
					case 0:
						addValue.priceType = "recurring";
						break;
					case 1:
						addValue.priceType = "one_time";
						break;
					case 2:
						addValue.priceType = "usage";
						break;
					case 3:
						addValue.priceType = "tariff";
						break;
				}
			}
			if(this.priceUpdatePla) {
				var plas = new Array();
				var plaRef = new Object();
				plaRef.href = this.priceUpdatePla;
				plas.push(plaRef);
				addValue.pricingLogicAlgorithm = plas;
			}
			if(this.priceUpdateUnits == "Seconds") {
				this.priceUpdateUnits = "s";
			}
			if(this.priceUpdateUnits == "Bytes") {
				this.priceUpdateUnits = "b";
			}
			if(this.priceUpdateUnits == "Messages") {
				this.priceUpdateUnits = "msg";
			}
			if(this.priceUpdateUnits && this.priceUpdateSize) {
				var m = this.priceUpdateSize.slice(-1);
				if(isNaN(parseInt(m))) {
					var s = this.priceUpdateSize.slice(0, -1);
				} else {
					var s = this.priceUpdateSize;
				}
				if(this.priceUpdateUnits == "b") {
					if (m == "m") {
						addValue.unitOfMeasure = s + "000000b";
					} else if(m == "g") {
						addValue.unitOfMeasure = s + "000000000b";
					} else if(m == "k") {
						addValue.unitOfMeasure = s + "000b";
					} else {
						addValue.unitOfMeasure = s + "b";
					}
				} else if(this.priceUpdateUnits == "s") {
					var n = Number(s);
					if(m == "m") {
						n = n * 60;
						addValue.unitOfMeasure = n.toString() + "s";
					} else if(m == "h") {
						n = n * 3600;
						addValue.unitOfMeasure = n.toString() + "s";
					} else {
						addValue.unitOfMeasure = n.toString() + "s";
					}
				} else if(this.priceUpdateUnits == "msg") {
					addValue.unitOfMeasure = s + "msg";
				}
			}
			if(this.priceUpdateAmount) {
				var priceAmount = new Object();
				priceAmount.taxIncludedAmount = this.priceUpdateAmount;
				addValue.price = priceAmount;
			}
			if(this.priceUpdatePeriod) {
				switch(this.$.updatePricePeriod.selected) {
					case 0:
						addValue.recurringChargePeriod = "hourly";
						break;
					case 1:
						addValue.recurringChargePeriod = "daily";
						break;
					case 2:
						addValue.recurringChargePeriod = "weekly";
						break;
					case 3:
						addValue.recurringChargePeriod = "monthly";
						break
					case 4:
						addValue.recurringChargePeriod = "yearly";
						break;
				}
			}
			if(this.priceUpdateCurrency) {
				addValue.currency = this.priceUpdateCurrency;
			}
			if(this.priceUpdateReserveTime) {
				addValue.radiusReserveTime = this.priceUpdateReserveTime;
			}
			if(this.priceUpdateReserveOctets) {
				addValue.radiusReserveOctets = this.priceUpdateReserveOctets;
			}
			var prodSpecCharValueUse = new Array();
			if (this.priceUpdateTariff) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "destPrefixTariffTable";
				charValueUse.minCardinality = 0;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				charValue.value = this.priceUpdateTariff;
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "5";
				prodSpec.href = "/catalogManagement/v2/productSpecification/5";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.priceUpdateRoamingTable) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "roamingTable";
				charValueUse.minCardinality = 0;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				charValue.value = this.priceUpdateRoamingTable;
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "5";
				prodSpec.href = "/catalogManagement/v2/productSpecification/5";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.priceUpdateChargingKey) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "chargingKey";
				charValue.value = parseInt(this.priceUpdateChargingKey);
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "3";
				prodSpec.href = "/catalogManagement/v2/productSpecification/3";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.priceUpdateReserveTime) {
				var charValue = new Object();
				charValue.unitOfMeasure = "seconds";
				var lastChar = this.priceUpdateReserveTime.slice(-1);
				if(isNaN(parseInt(lastChar))) {
					var s = this.priceUpdateReserveTime.slice(0, -1);
				} else {
					var s = this.priceUpdateReserveTime;
				}
				if(charValue.unitOfMeasure == "seconds") {
					var n = Number(s);
					if(lastChar == "m") {
						charValue.value = n * 60;
					}
					else if(lastChar == "h") {
						charValue.value = n * 3600;
					} else {
						charValue.value = n;
					}
				}
				var charValueUse = new Object();
				charValueUse.name = "radiusReserveTime";
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "1";
				prodSpec.href = "/catalogManagement/v2/productSpecification/1";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.priceUpdateReserveOctets) {
				var charValue = new Object();
				charValue.unitOfMeasure = "octets";
				var lastChar = this.priceUpdateReserveOctets.slice(-1);
				if(isNaN(parseInt(lastChar))) {
					var s = this.priceUpdateReserveOctets.slice(0, -1);
				} else {
					var s = item.reserveBytes;
				}
				if(charValue.unitOfMeasure == "octets") {
					var n = Number(s);
					if(lastChar == "g") {
						charValue.value = n * 1000000000;
					}
					else if(lastChar == "m") {
						charValue.value = n * 1000000;
					}
					else if(lastChar == "k") {
						charValue.value = n * 1000;
					} else {
						charValue.value = n;
					}
				}
				var charValueUse = new Object();
				charValueUse.name = "radiusReserveOctets";
				var charValuesByte = new Array();
				charValuesByte.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValuesByte;
				var prodSpec = new Object();
				prodSpec.id = "1";
				prodSpec.href = "/catalogManagement/v2/productSpecification/1";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.priceUpdateTODStart || this.priceUpdateTODEnd) {
				if (this.priceUpdateTODStart.length > 0 || this.priceUpdateTODEnd.length > 0) {
					var charValueUse = new Object();
					charValueUse.name = "timeOfDayRange";
					charValueUse.valueType = "Range";
					charValueUse.minCardinality = 0;
					charValueUse.maxCardinality = 1;
					var charValue1 = new Object();
					var charValue = new Object();
					var charValueLower = new Object();
					var epochStart = new Date("1970-01-01T" + this.priceUpdateTODStart);
					charValueLower.amount = (epochStart.getHours() * 60) + epochStart.getMinutes();
					charValueLower.units = "minutes";
					charValue.lowerValue = charValueLower;
					var charValueUpper = new Object();
					var epochEnd = new Date("1970-01-01T" + this.priceUpdateTODEnd);
					charValueUpper.amount = (epochEnd.getHours() * 60) + epochEnd.getMinutes();
					charValueUpper.units = "minutes";
					charValue.upperValue = charValueUpper;
					charValue1.value = charValue;
					var charValues = new Array();
					charValues.push(charValue1);
					charValueUse.productSpecCharacteristicValue = charValues;
					prodSpecCharValueUse.push(charValueUse);
					addValue.prodSpecCharValueUse = prodSpecCharValueUse;
				}
			}
			createPrice.value = addValue;
			updatePriceNew1.push(createPrice);
			ajax.body = JSON.stringify(updatePriceNew1);
			ajax.generateRequest();
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	_updateAddAlteration() {
		function priceUpdateCallDirAltName(updateAlts) {
			return updateAlts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;
		}
		var updateIndexAlt = this.alterations.findIndex(priceUpdateCallDirAltName);
		if(updateIndexAlt == -1) {
			var updateAltNew = new Object();
		} else {
			var updateAltNew = this.alterations[updateIndexAlt];
		}
		updateAltNew.name = this.$.updateAltName.value;
		updateAltNew.description = this.altUpdateDescription;
		updateAltNew.start = this.updateOfferStartDateAlt;
		updateAltNew.end = this.updateOfferEndDateAlt;
		switch(this.$.updateAltType.selected) {
			case 0:
				updateAltNew.priceType = "recurring";
				break;
			case 1:
				updateAltNew.priceType = "one_time";
				break;
			case 2:
				updateAltNew.priceType = "usage";
				break;
		}
		switch(this.$.updateAlterationUnits.selected) {
			case 0:
				updateAltNew.unit = "b";
				break;
			case 1:
				updateAltNew.unit = "c";
				break
			case 2:
				updateAltNew.unit = "s";
				break;
		}
		updateAltNew.size = this.$.updateAltSize.value;
		switch(this.$.updateAltPeriod.selected) {
			case 0:
				updateAltNew.period = "hourly";
				break;
			case 1:
				updateAltNew.period = "daily";
				break;
			case 2:
				updateAltNew.period = "weekly";
				break;
			case 3:
				updateAltNew.period = "monthly";
				break
			case 4:
				updateAltNew.period = "yearly";
				break;
		}
		updateAltNew.currency = this.altUpdateCurrency;
		updateAltNew.amount= this.$.updateAltAmount.value;
		if(updateAltNew.name
					&& updateAltNew.priceType
					&& updateAltNew.unit
					&& (updateAltNew.amount || updateAltNew.amount == 0)) {
			if(updateIndexAlt == -1) {
				this.push('alterations', updateAltNew);
			}
			this.altUpdateName = null
			this.altUpdateDescription = null;
			this.updateOfferStartDateAlt = null;
			this.updateOfferEndDateAlt = null;
			this.altUpdateType = null;
			this.$.updateAltSize.value = null;
			this.altUpdateCurrency = null;
			this.$.updateAltAmount.value = null;
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	_deleteOffer(event) {
		this.$.deleteOfferAjax.url = "/catalogManagement/v2/productOffering/"
					+ this.updateOfferName;
		this.$.deleteOfferAjax.generateRequest();
	}

	_deleteOfferResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.cancelDialog();
	}

	_deleteOfferError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_onLoadingChanged(event) {
		if (this.$.updateProductOfferAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}

	cancelDialog() {
		this.set('prices', []);
		this.set('alterations', []);
		var list = document.getElementsByClassName("bundleCheck");
		Array.prototype.forEach.call(list, function(el) {
			if(el.checked == true) {
				el.checked = false;
			}
		});
		this.updateReserveSessionTime = null;
		this.$.updateReserveSessionTime.disabled = false;
		this.updateReserveSessionOctets = null;
		this.$.updateReserveSessionOctets.disabled = false;
		this.updateOfferName = null;
		this.updateOfferDescription = null;
		this.updateOfferSpecification = null;
		this.updateOfferStartDatee = null;
		this.updateOfferEndDate = null;
		this.offerUpdateStatus = null;
		this.$.updateOfferStatus.selected = null;
		this.$.updateAddPriceChars.hide();
		this.$.addBundleUpdate.hide();
		this.priceUpdateName = null;
		this.priceUpdateDescription = null;
		this.updateOfferStartDatePrice = null;
		this.updateOfferEndDatePrice = null;
		this.priceUpdateType = null;
		this.$.priceBytes.disabled = false;
		this.$.priceSeconds.disabled = true;
		this.priceUpdatePla = null;
		this.priceUpdateUnits = null;
		this.priceUpdateAmount = null;
		this.priceUpdateSize = null;
		this.priceUpdateCurrency = null;
		this.priceUpdatePeriod = null;
		this.priceUpdateAlteration = null;
		this.altUpdateName = null;
		this.altUpdateDescription = null;
		this.updateOfferStartDateAlt = null;
		this.updateOfferEndDateAlt = null;
		this.altUpdateType = null;
		this.$.altSeconds.disabled = false;
		this.$.altBytes.disabled = false;
		this.$.updateAltSize.value = null;
		this.altUpdateCurrency = null;
		this.$.updateAltAmount.value = null;
		this.altUpdatePeriod = null;
		this.altUpdateUnits = null;
		this.priceUpdateReserveTime = null;
		this.$.priceUpdateReserveTimeInput.disabled = false;
		this.priceUpdateReserveOctets = null;
		this.$.priceUpdateReserveOctetsInput.disabled = false;
		this.priceUpdateTariff = null;
		this.priceUpdateRoamingTable = null;
		this.priceUpdateChargingKey = null;
		this.updateRedirect = null;
		this.priceUpdateTODStart = null;
		this.priceUpdateTODEnd = null;
		this.priceUpdateCallDirIn = false;
		this.priceUpdateCallDirOut = false;
		this.selected = 0;
		this.$.updateAddPriceOfferChars.hide();
		this.$.updateOnClickOfferChars.icon="arrow-drop-down"
		this.$.updateAddPriceChars.hide();
		this.$.updateOnClickChars.icon="arrow-drop-down"
		this.$.updateAddPriceChars.hide();
		this.$.updateOnClickCharsTime.icon="arrow-drop-down"
		this.$.updateOfferModal.close();
	}

	timeCharValue(time) {
		var charValue = new Object();
		var last = time.slice(-1);
		if(last == "s") {
			charValue.unitOfMeasure = "seconds";
			charValue.value = parseInt(time.slice(0, -1));
		} else if(last == "m") {
			charValue.unitOfMeasure = "minutes";
			charValue.value = parseInt(time.slice(0, -1));
		} else {
			charValue.unitOfMeasure = "seconds";
			charValue.value = parseInt(time);
		}
		return charValue;
	}

	octetsCharValue(octets) {
		var charValue = new Object();
		var last = octets.slice(-1);
		if(last == "b") {
			charValue.unitOfMeasure = "bytes";
			charValue.value = parseInt(octets.slice(0, -1));
		} if(last == "k") {
			charValue.unitOfMeasure = "kilobytes";
			charValue.value = parseInt(octets.slice(0, -1));
		} else if(last == "m") {
			charValue.unitOfMeasure = "megabytes";
			charValue.value = parseInt(octets.slice(0, -1));
		} else if(last == "g") {
			charValue.unitOfMeasure = "gigabytes";
			charValue.value = parseInt(octets.slice(0, -1));
		} else {
			charValue.unitOfMeasure = "bytes";
			charValue.value = parseInt(octets);
		}
		return charValue;
	}
}

window.customElements.define('sig-offer-update', offerUpdate);
