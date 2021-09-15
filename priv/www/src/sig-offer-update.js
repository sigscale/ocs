/**
 * Copyright 2016 - 2021 SigScale Global Inc.
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
					<div id="addOffer-tab">
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
										id="updateReserveSession"
										allowed-pattern="[0-9mh]"
										pattern="^[0-9]+[mh]?$"
										auto-validate
										label="RADIUS Reserve Session"
										value=0>
								</paper-input>
								<paper-tooltip>
										Add a value to update the offer reserve session
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
										characteristic serviceIdentifiers
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										allowed-pattern="[0-9\.]"
										label="Redirect Server"
										value="{{updateRedirect}}">
								</paper-input>
								<paper-tooltip>
										Add a value to update the offer reserve session
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
					<div id=updatePrice-tab>
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
									on-selected-item-changed="checkRecure">
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
									id="updatePriceSize"
									value="{{priceUpdateSize}}"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									label="Unit Size"
									auto-validate>
							</paper-input>
							<paper-tooltip>
									Unit of measure (Bytes=(MB="m", GB="g", KB="k"), Seconds=(Minutes="m",Hour="h"), Messages=(Messages="msg"))
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
								<span>Time of day range</span>
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
											id="updateTimeOfDayStart"
											value="{{startTimeUpdate}}"
											label="Start Time">
									</paper-input>
									<paper-tooltip>
											Add a value to update the offer price start time of day
									</paper-tooltip>
								</div>
								<div>
									<paper-input
											type="time"
											id="updateTimeOfDayEnd"
											value="{{endTimeUpdate}}"
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
							<iron-collapse id="updateAddCall">
								<paper-checkbox
										id="updateCheckIn"
										value="{{priceUpdateCheckIn}}">
									Incoming
								</paper-checkbox>
								<paper-checkbox
										id="updateCheckOut"
										value="{{priceUpdateCheckOut}}">
									Outgoing
								</paper-checkbox>
							</iron-collapse>
							<div>
								<paper-input
										id="updateAddPriceCharReserveTime"
										type="number"
										label="RADIUS Reserve Time"
										value=0>
								</paper-input>
								<paper-tooltip>
										Add a value to update the offer price characteristic reserve time
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										id="updateAddPriceCharReserveBytes"
										type="number"
										label="RADIUS Reserve Data"
										value=0>
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
										value="{{priceAddRoaming}}"
										label="Roaming Table">
								</paper-input>
								<paper-tooltip>
										Add a value to update the offer price roaming table
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										value="{{chargingKey}}"
										type="number"
										label="Charging Key">
								</paper-input>
								<paper-tooltip>
										Add a value to update the offer price charging key
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
									on-tap="updateAddPrice">
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
					<div id="add-Alt-tab">
						<div>
							<datalist id="updateAltNames">
								<template is="dom-repeat" items="{{alterations}}">
									<option value="{{item.name}}"/>
								</template>
							</datalist>
							<input id="updateAltName" 
									list="updateAltNames" 
									value="{{AltUpdateName::input}}"
									placeholder="Name"/>
							<paper-tooltip
									for="updateAltName"
									offset="0">
								Add a value to update the offer price alteration name
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{AltUpdateDescription}}"
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
									on-selected-item-changed="checkRecureAlt">
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
									Unit of measure (Bytes=(MB="m", GB="g", KB="k"), Seconds=(Minutes="m",Hour="h"), Messages=(Messages="msg"))
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									id="updateAltsUnitsdrop"
									value= "{{altUpdateUnits}}"
									no-animations="true"
									label="Units">
								<paper-listbox
										id="updateUnitDrop"
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
									value="{{AltUpdatePeriod}}">
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
									on-tap="updateAddAlteration">
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
			listeners: {
				'updateCheckIn.checked-changed': 'updateCheckInChanged',
				'updateCheckOut.checked-changed': 'updateCheckOutChanged',
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			priceUpdateName: {
				observer: 'updatePriceDialog'
			},
			AltUpdateName: {
				observer: 'updateAddAltsDialog'
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
			characteristics: {
				type: Array,
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
			updateOfferStartDate: {
				type: String
			},
			updateRedirect: {
				type: String
			},
			serviceIdentifier: {
				type: Number
			},
			updateOfferEndDate: {
				type: String
			},
			priceUpdateDescription: {
				type: String
			},
			chargingKey: {
				type: Number
			},
			updateOfferStartDatePrice: {
				type: String
			},
			updateOfferEndDatePrice: {
				type: String
			},
			priceUpdateSize: {
				type: String
			},
			priceUpdateCurrency: {
				type: String
			},
			startTimeUpdate: {
				type: String
			},
			endTimeUpdate: {
				type: String
			},
			priceUpdateTariff: {
				type: String
			},
			AltUpdateDescription: {
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
			this.updateOfferStartDate = current.startDate;
			this.updateOfferEndDate = current.endDate;
			this.offerUpdateStatus = current.lifecycleStatus;
			this.characteristics = current.prodSpecCharValueUse;
			for (var indexCha in current.prodSpecCharValueUse) {
				if(current.prodSpecCharValueUse[indexCha].name == "radiusReserveSessionTime") {
					this.$.updateReserveSession.value = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
				if(current.prodSpecCharValueUse[indexCha].name == "redirectServer") {
					this.updateRedirect = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
				if(current.prodSpecCharValueUse[indexCha].name == "serviceIdentifier") {
					this.serviceIdentifier = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
			}
			for(var index in current.prices) {
				var newPrice = new Object();
				newPrice.name = current.prices[index].name;
				newPrice.description = current.prices[index].description;
				if(current.prices[index].validFor) {
					newPrice.start = current.prices[index].validFor.startDateTime;
					newPrice.end = current.prices[index].validFor.endDateTime;
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
				if (current.prices[index].unitOfMeasure) {
					var unitOfMeasure = current.prices[index].unitOfMeasure;
					switch(unitOfMeasure.charAt(unitOfMeasure.length - 1)) {
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
							specChar[indexChar] = {name: "radiusReserveTime", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "radiusReserveOctets") {
							specChar[indexChar] = {name: "radiusReserveOctets", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
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
							switch(unitOfMeasure.charAt(unitOfMeasure.length - 1)) {
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
				function checkExist(price) {
					return price.name == current.prices[index].name;
				}
				if(!this.prices.some(checkExist)) {
					this.push('prices', newPrice);
				}
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
		var bundle = document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('offerGrid').activeItem.bundledProductOffering;
		for(var indexBundle in bundle) {
			function checkExistProduct(prod) {
				return prod.name == bundle[indexBundle].name;
			}
			var indexProduct = this.offers.findIndex(checkExistProduct);
			if (indexProduct != -1) {
				this.offers[indexProduct].checked = true;
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
		} else {
			this.$.updateAddPriceOfferChars.hide();
			this.$.updateOnClickOfferChars.icon="arrow-drop-down"
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

	updateCheckInChanged(event) {
		if(event.detail.value) {
			this.$.updateCheckOut.checked = false;
		}
	}

	updateCheckOutChanged(event) {
		if(event.detail.value) {
			this.$.updateCheckIn.checked = false;
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

	updatePriceDialog() {
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
				this.priceUpdateSize = null;
				this.priceUpdateUnits = null;
				this.priceUpdateAmount = null;
				this.priceUpdateCurrency = null;
				this.priceUpdatePeriod = null;
				this.$.updateAddPriceCharReserveTime.value = null;
				this.$.updateAddPriceCharReserveBytes.value = null;
				this.$.updateReserveSession.value = null;
				this.updateRedirect = null;
				this.serviceIdentifier = null;
				this.priceAddRoaming = null;
				this.chargingKey = null;
				this.priceUpdateTariff = null;
				this.priceUpdatePolicy = null;
				this.startTimeUpdate = null;
				this.endTimeUpdate = null;
				this.$.updateCheckIn.checked = false;
				this.$.updateCheckOut.checked = false;
				this.priceUpdateAlteration = null;
			} else {
				this.priceUpdateDescription = this.prices[indexUpdatePrice].description;
				if(this.prices[indexUpdatePrice].start || this.prices[indexUpdatePrice].end) {
					this.updateOfferStartDatePrice = this.prices[indexUpdatePrice].start;
					this.updateOfferEndDatePrice = this.prices[indexUpdatePrice].end;
				}
				this.priceUpdateType = this.prices[indexUpdatePrice].priceType;
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
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "destPrefixTariffTable") {
							this.priceUpdateTariff = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "roamingTable") {
							this.priceAddRoaming = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "chargingKey") {
							this.chargingKey = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "radiusReserveTime") {
							this.$.updateAddPriceCharReserveTime.value = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "radiusReserveOctets") {
							this.$.updateAddPriceCharReserveBytes.value = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "timeOfDayRange") {
							this.startTimeUpdate = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.lowerValue.amount;
							this.endTimeUpdate = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.upperValue.amount;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "callDirection") {
							if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value == "originate") {
								this.$.updateCheckOut.checked = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
							} else if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value == "answer") {
								this.$.updateCheckIn.checked = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
							}
						}
					}
				}
			}
		}
	}

	updateAddAltsDialog() {
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
				this.AltUpdateDescription = null;
				this.updateOfferStartDateAlt = null;
				this.updateOfferEndDateAlt = null;
				this.altUpdateType = null;
				this.$.updateAltSize.value = null;
				this.altUpdateUnits = null;
				this.$.updateAltAmount.value = null;
				this.altUpdateCurrency = null;
				this.AltUpdatePeriod = null;
			} else {
				this.AltUpdateDescription = this.alterations[indexAlt].description;
				if(this.alterations[indexAlt].start || this.alterations[indexAlt].end) {
					this.updateOfferStartDateAlt = this.alterations[indexAlt].start;
					this.updateOfferEndDateAlt = this.alterations[indexAlt].end;
				}
				this.altUpdateType = this.alterations[indexAlt].priceType;
				this.$.updateAltSize.value = this.alterations[indexAlt].size;
				switch(this.alterations[indexAlt].unit) {
					case "b":
						this.$.updateUnitDrop.selected = 0;
						break;
					case "c":
						this.$.updateUnitDrop.selected = 1;
						break;
					case "s":
						this.$.updateUnitDrop.selected = 2;
						break;
				}
				if(this.alterations[indexAlt].currency || this.alterations[indexAlt].amount) {
					this.altUpdateCurrency = this.alterations[indexAlt].currency;
					this.$.updateAltAmount.value = this.alterations[indexAlt].amount;
				}
				this.AltUpdatePeriod = this.alterations[indexAlt].period;
			}
		}
	}

	_updateProductOffer(event) {
		var ajax =  this.$.updateProductOfferAjax;
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName; 
		var offerNew = new Array();
			if(this.updateOfferDescription == "") {
				var offerDescDel = new Object();
				offerDescDel.op = "remove";
				offerDescDel.path = "/description";
				offerNew.push(offerDescDel);
			} else if(this.updateOfferDescription) {
				var offerDesc = new Object();
				offerDesc.op = "add";
				offerDesc.path = "/description";
				offerDesc.value = this.updateOfferDescription;
				offerNew.push(offerDesc);
			}
			if(this.updateOfferStartDate == "") {
				var startDateTimeDel = new Object();
				startDateTimeDel.op = "remove";
				startDateTimeDel.path = "/description";
				offerNew.push(startDateTimeDel);
			} else if(this.updateOfferStartDate) {
				var startDateTimeObject = new Object();
				startDateTimeObject.op = "add";
				startDateTimeObject.path = "/validFor/startDateTime";
				startDateTimeObject.value = this.updateOfferStartDate;
				offerNew.push(startDateTimeObject);
			}
			if(this.updateOfferEndDate == "") {
				var endDateTimeDel = new Object();
				endDateTimeDel.op = "remove";
				endDateTimeDel.path = "/description";
				offerNew.push(endDateTimeDel);
			} else if(this.updateOfferEndDate){
				var endDateTimeObject = new Object();
				endDateTimeObject.op = "add";
				endDateTimeObject.path = "/validFor/endDateTime";
				endDateTimeObject.value = this.updateOfferEndDate;
				offerNew.push(endDateTimeObject);
			}
			if(this.offerUpdateStatus) {
				var stat = new Object();
				stat.op = "add";
				stat.path = "/lifecycleStatus";
				stat.value = this.offerUpdateStatus;
				offerNew.push(stat);
			}
		if(this.$.updateReserveSession.value) {
			function checkName(char) {
				return char.name == "radiusReserveSessionTime";
			}
			var res = this.characteristics.findIndex(checkName);
			if(res == -1) {
				var indexChar = "-";
				var reserveSession = new Object();
				reserveSession.op = "add";
				reserveSession.path = "/prodSpecCharValueUse/" + indexChar; 
				var session2Arr = new Array();
				var session2 = new Object();
				session2.default = true;
				session2.value = parseInt(this.$.updateReserveSession.value);
				session2Arr.push(session2);
				var session1 = new Object();
				session1.name = "radiusReserveSessionTime";
				session1.minCardinality = 0;
				session1.maxCardinality = 1;
				session1.productSpecCharacteristicValue = session2Arr;
				var session2 = new Object();
				session2.id = "1";
				session2.href = "/catalogManagement/v2/productSpecification/1";
				session1.productSpecification = session2;
				reserveSession.value = session1;
				offerNew.push(reserveSession);
			} else {
				var indexChar = res.toString();
				var reserveSession = new Object();
				reserveSession.op = "replace";
				reserveSession.path = "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
				reserveSession.value = parseInt(this.$.updateReserveSession.value);
				offerNew.push(reserveSession);
			}
		}
		function checkNameRe(redirect) {
			return redirect.name == "redirectServer";
		}
		var res = this.characteristics.findIndex(checkNameRe);
		if(res == -1) {
			var indexChar = "-";
			var redirectSer = new Object();
			redirectSer.op = "add";
			redirectSer.path = "/prodSpecCharValueUse/" + indexChar; 
			var redirectSerArr = new Array();
			var redirectSer1 = new Object();
			redirectSer1.value = this.updateRedirect;
			redirectSerArr.push(redirectSer1);
			var redirectSer2 = new Object();
			redirectSer2.name = "redirectServer";
			redirectSer2.minCardinality = 0;
			redirectSer2.maxCardinality = 1;
			redirectSer2.productSpecCharacteristicValue = redirectSerArr;
			var redirectSer1 = new Object();
			redirectSer1.id = "8";
			redirectSer1.href = "/catalogManagement/v2/productSpecification/8";
			redirectSer2.productSpecification = redirectSer1;
			redirectSer.value = redirectSer2;
			offerNew.push(redirectSer);
		} else {
			var indexChar = res.toString();
			var redirectServer = new Object();
			redirectServer.op = "replace";
			redirectServer.path = "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
			redirectServer.value = this.updateRedirect;
			if(redirectServer.value == "") {
				var redirectServerDel = new Object();
				redirectServerDel.op = "remove";
				redirectServerDel.path = "/prodSpecCharValueUse/" + indexChar;
				offerNew.push(redirectServerDel);
			} else {
				offerNew.push(redirectServer);
			}
		}
		if(this.serviceIdentifier) {
			function checkNameService(serviceId) {
				return serviceId.name == "serviceIdentifier";
			}
			var resService = this.characteristics.findIndex(checkNameService);
			if(resService == -1) {
				var indexChar = "-";
				var Service = new Object();
				Service.op = "add";
				Service.path = "/prodSpecCharValueUse/" + indexChar; 
				var ServiceArr = new Array();
				var ServiceObj1 = new Object();
				ServiceObj1.value = this.serviceIdentifier;
				ServiceArr.push(ServiceObj1);
				var ServiceObj2 = new Object();
				ServiceObj2.name = "serviceIdentifier";
				ServiceObj2.minCardinality = 0;
				ServiceObj2.maxCardinality = 1;
				ServiceObj2.productSpecCharacteristicValue = ServiceArr;
				var ServiceObj1 = new Object();
				ServiceObj1.id = "8";
				ServiceObj1.href = "/catalogManagement/v2/productSpecification/8";
				ServiceObj2.productSpecification = ServiceObj1;
				Service.value = ServiceObj2;
				offerNew.push(Service);
			} else {
				var indexChar = resService.toString();
				var Service = new Object();
				Service.op = "replace";
				Service.path = "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
				Service.value = this.serviceIdentifier;
				if(Service.value == "") {
					var serviceDel = new Object();
					serviceDel.op = "remove";
					serviceDel.path = "/prodSpecCharValueUse/" + indexChar;
					offerNew.push(serviceDel);
				} else {
					offerNew.push(Service);
				}
			}
		}
		if(this.priceUpdatePolicy) {
			function checkNameSPolicy(policyId) {
				return policyId.name == "policyTable";
			}
			var resPolicy = this.characteristics.findIndex(checkNameSPolicy);
			if(resPolicy == -1) {
				var indexChar = "-";
				var policy = new Object();
				policy.op = "add";
				policy.path = "/prodSpecCharValueUse/" + indexChar; 
				var policyArr = new Array();
				var policyObj1 = new Object();
				policyObj1.value = this.priceUpdatePolicy;
				policyArr.push(policyObj1);
				var policyObj2 = new Object();
				policyObj2.name = "policyTable";
				policyObj2.minCardinality = 0;
				policyObj2.maxCardinality = 1;
				policyObj2.productSpecCharacteristicValue = policyArr;
				var policyObj1 = new Object();
				policyObj1.id = "3";
				policyObj1.href = "/catalogManagement/v2/productSpecification/3";
				policyObj2.productSpecification = policyObj1;
				policy.value = policyObj2;
				offerNew.push(policy);
			} else {
				var indexChar = resPolicy.toString();
				var policy = new Object();
				policy.op = "replace";
				policy.path = "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
				policy.value = this.priceUpdatePolicy;
				if(policy.value == "") {
					var policyDel = new Object();
					policyDel.op = "remove";
					policyDel.path = "/prodSpecCharValueUse/" + indexChar;
					offerNew.push(policyDel);
				} else {
					offerNew.push(policy);
				}
			}
		}
		ajax.body = JSON.stringify(offerNew);
		ajax.generateRequest();
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
		if(this.priceUpdateDescription != this.prices[indexPrices].description) {
			var priceDesc = new Object();
			priceDesc.op = "add";
			priceDesc.path = "/productOfferingPrice/" + indexPrices + "/description";
			priceDesc.value = this.priceUpdateDescription;
			if(priceDesc.value == "") {
				var priceDescDel = new Object();
				priceDescDel.op = "remove";
				priceDescDel.path = "/productOfferingPrice/" + indexPrices + "/description";
				updatePriceNew.push(priceDescDel);
			} else {
				updatePriceNew.push(priceDesc);
			}
		}
		if(this.priceUpdateType != this.prices[indexPrices].priceType) {
			var pricetype = new Object();
			pricetype.op = "add";
			pricetype.path = "/productOfferingPrice/" + indexPrices + "/priceType";
			switch(this.priceUpdateType) {
				case "Recurring":
					pricetype.value = "recurring";
					break;
				case "One Time":
					pricetype.value = "one_time";
					break;
				case "Usage":
					pricetype.value = "usage";
					break;
				case "Tariff":
					pricetype.value = "tariff";
					break;
			}
			if(pricetype.value == "") {
				var priceTypeDel = new Object();
				priceTypeDel.op = "remove";
				priceTypeDel.path = "/productOfferingPrice/" + indexPrices + "/priceType";
				updatePriceNew.push(priceTypeDel);
			} else {
				updatePriceNew.push(pricetype);
			}
		} 
		if(this.priceUpdateSize != this.prices[indexPrices].size) {
			var priceSize = new Object();
			priceSize.op = "add";
			priceSize.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
			for(var indexUnit in this.prices) {
				if(this.priceUpdateUnits == "Seconds") {
					this.prices[indexUnit].unit = "s";
				}
				if(this.priceUpdateUnits == "Bytes") {
					this.prices[indexUnit].unit = "b";
				}
				if(this.prices[indexUnit].unit != undefined) {
					var unitDrop = this.prices[indexUnit].unit;
				}
				if(this.priceUpdateSize != undefined) {
					var sizeVal = this.priceUpdateSize;
				}
				if(unitDrop && sizeVal) {
					var len = sizeVal.length;
					var m = sizeVal.charAt(len - 1);
					if(isNaN(parseInt(m))) {
						var s = sizeVal.slice(0, (len - 1));
					} else {
						var s = sizeVal;
					}
					if(unitDrop == "b") {
						if (m == "m") {
							priceSize.value = s + "000000b";
						} else if(m == "g") {
							priceSize.value = s + "000000000b";
						} else if(m == "k") {
							priceSize.value = s + "000b";
						} else {
							priceSize.value = s + "b";
						}
					} else if(unitDrop == "s") {
						var n = Number(s);
						if(m == "m") {
							n = n * 60;
							priceSize.value = n.toString() + "s";
						} else if(m == "h") {
							n = n * 3600;
							priceSize.value = n.toString() + "s";
						} else {
							priceSize.value = n.toString() + "s";
						}
					}
				}
				if(priceSize.value == "") {
					var priceSizeDel = new Object();
					priceSizeDel.op = "remove";
					priceSizeDel.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
					updatePriceNew.push(priceSizeDel);
				} else {
					updatePriceNew.push(priceSize);
				}
			}
		}
		if(this.priceUpdateAmount.length != "") {
			if(this.priceUpdateAmount != this.prices[indexPrices].amount) {
				var priceAmount = new Object();
				priceAmount.op = "add";
				priceAmount.path = "/productOfferingPrice/" + indexPrices + "/price/taxIncludedAmount";
				priceAmount.value = this.priceUpdateAmount;
				updatePriceNew.push(priceAmount);
			}
		}
		if(this.priceUpdatePeriod != this.prices[indexPrices].period) {
			if(!this.$.updatePricePerioddrop.disabled) {
				var priceCharge = new Object();
				priceCharge.op = "add";
				priceCharge.path = "/productOfferingPrice/" + indexPrices + "/recurringChargePeriod";
				switch(this.priceUpdatePeriod) {
					case "Hourly":
						priceCharge.value = "hourly";
						break;
					case "Daily":
						priceCharge.value = "daily";
						break;
					case "Weekly":
						priceCharge.value = "weekly";
						break;
					case "Monthly":
						priceCharge.value = "monthly";
						break;
					case "Yearly":
						priceCharge.value = "yearly";
						break;
				}
				if(priceCharge.value == "") {
					var priceChargeDel = new Object();
					priceChargeDel.op = "remove";
					priceChargeDel.path = "/productOfferingPrice/" + indexPrices + "/recurringChargePeriod";
					updatePriceNew.push(priceChargeDel);
				} else {
					updatePriceNew.push(priceCharge);
				}
			} 
		}
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			function checkChar1(charVal) {
				return charVal.name == "radiusReserveTime";
			}
			var res = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChar1);
			if(res == -1) {
				var indexChar = "-";
				var charReserve = new Object();
				charReserve.op = "add";
				charReserve.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar;
				var resTime1 = new Object();
				resTime1.name = "radiusReserveTime";
				resTime1.valueType = "Number";
				resTime1.minCardinality = 1;
				resTime1.maxCardinality = 1;
				var resTime2Arr = new Array();
				var resTime2 = new Object();
				resTime2.unitOfMeasure = "seconds";
				resTime2.default = true;
				resTime2.value = this.$.updateAddPriceCharReserveTime.value;
				resTime2Arr.push(resTime2);
				resTime1.productSpecCharacteristicValue = resTime2Arr;
				var resTime3 = new Object();
				resTime3.id = "4";
				resTime3.href = "/catalogManagement/v2/productSpecification/4";
				resTime1.productSpecification = resTime3;
				charReserve.value = resTime1;
				updatePriceNew.push(charReserve);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) { 
				if(this.$.updateAddPriceCharReserveTime.value != this.prices[indexPrices].prodSpecCharValueUse[res].value) {
					var indexChar = res.toString();
					var charReserve = new Object();
					charReserve.op = "add";
					charReserve.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
					charReserve.value = this.$.updateAddPriceCharReserveTime.value;
					updatePriceNew.push(charReserve);
				}
			}
			function checkTar(char) {
				return char.name == "destPrefixTariffTable";
			}
			var resTarriff = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkTar);
			if(resTarriff == -1) {
				var indexCharTariff  = "-";
				var destTariff = new Object();
				destTariff.op = "add";
				destTariff.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharTariff;
				var tariff1 = new Object();
				tariff1.name = "destPrefixTariffTable";
				tariff1.minCardinality = 0;
				tariff1.maxCardinality = 1;
				var tariff2Arr = new Array();
				var tariff2 = new Object();
				tariff2.default = true;
				tariff2.value = this.priceUpdateTariff;
				tariff2Arr.push(tariff2);
				tariff1.productSpecCharacteristicValue = tariff2Arr;
				var tariff3 = new Object();
				tariff3.id = "3";
				tariff3.href = "/catalogManagement/v2/productSpecification/3";
				tariff1.productSpecification = tariff3;
				destTariff.value = tariff1;
				updatePriceNew.push(destTariff);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) { 
				if(this.priceUpdateTariff != this.prices[indexPrices].prodSpecCharValueUse[resTarriff].value) {
					var indexCharTariff = resTarriff.toString();
					var destTariff = new Object();
					destTariff.op = "add";
					destTariff.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharTariff  + "/productSpecCharacteristicValue/0/value";
					destTariff.value = this.priceUpdateTariff;
					if(destTariff.value == "") {
						var priceTariffDel = new Object();
						priceTariffDel.op = "remove";
						priceTariffDel.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharCharging;
						updatePriceNew.push(priceTariffDel);
					} else {
						updatePriceNew.push(destTariff);
					}
				}
			}
			function checkCharge(charge) {
				return charge.name == "chargingKey";
			}
			var resCharge = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCharge);
			if(resCharge == -1) {
				var indexCharCharging  = "-";
				var key = new Object();
				key.op = "add";
				key.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharCharging;
				var key1 = new Object();
				key1.name = "chargingKey";
				var key2Arr = new Array();
				var key2 = new Object();
				if(!isNaN(parseInt(this.chargingKey))) {
					key2.value = parseInt(this.chargingKey);
				}
				key2Arr.push(key2);
				key1.productSpecCharacteristicValue = key2Arr;
				var key3 = new Object();
				key3.id = "3";
				key3.href = "/catalogManagement/v2/productSpecification/3";
				key1.productSpecification = key3;
				key.value = key1;
				updatePriceNew.push(key);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
				if(this.chargingKey != this.prices[indexPrices].prodSpecCharValueUse[resCharge].value) {
					var indexCharCharging = resCharge;
					var keyE = new Object();
					keyE.op = "add";
					keyE.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharCharging  + "/productSpecCharacteristicValue/0/value";
					keyE.value = parseInt(this.chargingKey);
					if(isNaN(keyE.value)) {
						var priceChargeDelete = new Object();
						priceChargeDelete.op = "remove";
						priceChargeDelete.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharCharging;
						updatePriceNew.push(priceChargeDelete);
					} else {
						updatePriceNew.push(keyE);
					}
				}
			}
			function checkRoaming(char) {
				return char.name == "roamingTable";
			}
			var resRoaming = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkRoaming);
			if(resRoaming == -1) {
				var indexCharRoaming = "-";
				var roam = new Object();
				roam.op = "add";
				roam.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharRoaming;
				var roam1 = new Object();
				roam1.name = "roamingTable";
				roam1.maxCardinality = 1;
				var roam2Arr = new Array();
				var roam2 = new Object();
				roam2.default = true;
				if(this.priceAddRoaming == null) {
					roam2.value = this.priceAddRoaming;
				}
				roam2Arr.push(roam2);
				roam1.productSpecCharacteristicValue = roam2Arr;
				var roam3 = new Object();
				roam3.id = "3";
				roam3.href = "/catalogManagement/v2/productSpecification/3";
				roam1.productSpecification = roam3;
				roam.value = roam1;
				updatePriceNew.push(roam);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
				if(this.priceAddRoaming != this.prices[indexPrices].prodSpecCharValueUse[resRoaming].value) {
					var indexCharRoaming = resRoaming.toString();
					var roamR = new Object();
					roamR.op = "add";
					roamR.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharRoaming  + "/productSpecCharacteristicValue/0/value";
					roamR.value = this.priceAddRoaming;
					if(roamR.value == "") {
						var priceRoomDel = new Object();
						priceRoomDel.op = "remove";
						priceRoomDel.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharRoaming;
						updatePriceNew.push(priceRoomDel);
					} else {
						updatePriceNew.push(roamR);
					}
				}
			}
			function checkCharOctet1(charVal) {
				return charVal.name == "radiusReserveOctets";
			}
			var resReserveOctets = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCharOctet1);
			if(resReserveOctets == -1) {
				var indexChar1 = "-";
				var charResBytes = new Object();
				charResBytes.op = "add";
				charResBytes.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar1;
				var resByte1 = new Object();
				resByte1.name = "radiusReserveOctets";
				resByte1.valueType = "Number";
				resByte1.minCardinality = 1;
				resByte1.maxCardinality = 1;
				var resByte2Arr = new Array();
				var resByte2 = new Object();
				resByte2.unitOfMeasure = "octets";
				resByte2.default = true;
				resByte2.value = this.$.updateAddPriceCharReserveBytes.value;
				resByte1.productSpecCharacteristicValue = resByte2Arr;
				var resByte3 = new Object();
				resByte3.id = "4";
				resByte3.href = "/catalogManagement/v2/productSpecification/4";
				resByte1.productSpecification = resByte3;
				resByte2Arr.push(resByte2);
				charResBytes.value = resByte1; 
				updatePriceNew.push(charResBytes);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
				if(this.$.updateAddPriceCharReserveBytes.value != this.prices[indexPrices].prodSpecCharValueUse[resReserveOctets].value) {
					var indexChar1 = resReserveOctets.toString();
					var charResBytes = new Object();
					charResBytes.op = "add";
					charResBytes.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar1 + "/productSpecCharacteristicValue/0/value";
					charResBytes.value = this.$.updateAddPriceCharReserveBytes.value;
					updatePriceNew.push(charResBytes);
				}
			}
		}
		if(this.$.updateCheckIn.checked || this.$.updateCheckOut.checked) {
			function checkCall1(callVal) {
				return callVal.name == "callDirection";
			}
			var resCall = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCall1);
			if(resCall == -1) {
				var indexCall1 = "-";
				var call = new Object();
				call.op = "add";
				call.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCall1;
				var callDir1 = new Object();
				callDir1.name = "callDirection";
				callDir1.minCardinality = 1;
				callDir1.maxCardinality = 1;
				var callDir2Arr = new Array();
				var callDir2 = new Object();
				callDir2.default = true;
				if(this.$.updateCheckIn.checked) {
					callDir2.value = "answer";
				} else if(this.$.updateCheckOut.checked) {
					callDir2.value = "originate";
				}
				callDir2Arr.push(callDir2);
				callDir1.productSpecCharacteristicValue = callDir2Arr;
				var callDir3 = new Object();
				callDir3.id = "5";
				callDir3.href = "/catalogManagement/v2/productSpecification/5";
				callDir1.productSpecification = callDir3;
				call.value = callDir1;
				updatePriceNew.push(call);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
				if(this.$.updateCheckIn.checked != this.prices[indexPrices].prodSpecCharValueUse[resCall].value || this.$.updateCheckOut.checked != this.prices[indexPrices].prodSpecCharValueUse[resCall].value) {
					var indexCall1 = resCall.toString();
					var call = new Object();
					call.op = "add";
					call.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCall1 + "/productSpecCharacteristicValue/0/value";
					if(this.$.updateCheckIn.checked) {
						call.value = "answer";
					} else if(this.$.updateCheckOut.checked) {
						call.value = "originate";
					}
					updatePriceNew.push(call);
				}
			}
		}
		if(this.startTimeUpdate ||
			this.endTimeUpdate ||
			(this.startTimeUpdate &&
			this.endTimeUpdate)) {
			function checkCharTime(charVal) {
				return charVal.name == "timeOfDayRange";
			}
			var resTime = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCharTime);
			if(resTime == -1) {
				var indexChar2 = "-";
				var timeDay = new Object();
				timeDay.op = "add";
				timeDay.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2;
				var timeRange1 = new Object();
				timeRange1.name = "timeOfDayRange";
				timeRange1.valueType = "Range";
				timeRange1.minCardinality = 0;
				timeRange1.maxCardinality = 1;
				var timeRangeArr = new Array();
				var timeRange2 = new Object();
				var timeRange3 = new Object();
				var timeRangeLower = new Object();
				timeRangeLower.amount = this.startTimeUpdate;
				timeRangeLower.units = "minutes";
				timeRange3.lowerValue = timeRangeLower;
				var timeRangeUpper = new Object();
				timeRangeUpper.amount = this.endTimeUpdate;
				timeRangeUpper.units = "minutes";
				timeRange3.upperValue = timeRangeUpper;
				timeRange2.value = timeRange3;
				timeRange1.productSpecCharacteristicValue = timeRangeArr;
				timeRangeArr.push(timeRange2);
				timeDay.value = timeRange1;
				updatePriceNew.push(timeDay);
			} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
				if(this.startTimeUpdate != this.prices[indexPrices].prodSpecCharValueUse[resTime].value || this.endTimeUpdate != this.prices[indexPrices].prodSpecCharValueUse[resTime].value) {
					var indexChar2 = resTime.toString();
					var timeDay1 = new Object();
					timeDay1.op = "add";
					timeDay1.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2 + "/productSpecCharacteristicValue/0/value/lowerValue/amount";
					timeDay1.value = this.startTimeUpdate;
					updatePriceNew.push(timeDay1);
					var timeDayEnd = new Object();
					timeDayEnd.op = "add";
					timeDayEnd.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2 + "/productSpecCharacteristicValue/0/value/upperValue/amount";
					timeDayEnd.value = this.endTimeUpdate;
					updatePriceNew.push(timeDayEnd);
				}
			}
		}
		ajax.body = JSON.stringify(updatePriceNew);
		ajax.generateRequest();
		this.priceUpdateDescription = null;
		this.priceUpdateSize = null;
		this.priceUpdateType = null;
		this.priceUpdatePeriod = null;
		this.$.updateAddPriceCharReserveTime.value = null;
		this.priceUpdateTariff = null;
		this.priceAddRoaming = null;
		this.chargingKey = null;
		this.$.updateAddPriceCharReserveBytes.value = null;
		this.$.updateCheckIn.checked = false;
		this.$.updateCheckOut.checked = false;
		this.startTimeUpdate = null;
		this.endTimeUpdate = null;
		this.priceUpdateAmount = null;
		this.priceUpdateUnits = null;
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
		if(this.AltUpdateDescription != this.alterations[indexAlt].description) {
			var alterationDesc = new Object();
			alterationDesc.op = "add";
			alterationDesc.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/description";
			alterationDesc.value = this.AltUpdateDescription;
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
					var len = sizeVal.length;
					var m = sizeVal.charAt(len - 1);
					if(isNaN(parseInt(m))) {
						var s = sizeVal.slice(0, (len - 1));
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
			this.$.updateAddPriceCharReserveBytes.disabled = false;
			this.$.updateAddPriceCharReserveTime.disabled = true;
		} else if(this.$.updatePriceUnits.selected == 1) {
			this.$.updatePriceSize.allowedPattern = "[0-9]";
			this.$.updatePriceSize.pattern = "^[0-9]+$";
			this.$.updatePriceSize.disabled = true;
		} else if(this.$.updatePriceUnits.selected == 2) {
			this.$.updatePriceSize.allowedPattern = "[0-9mh]";
			this.$.updatePriceSize.pattern = "^[0-9]+[mh]?$";
			this.$.updateAddPriceCharReserveTime.disabled = false;
			this.$.updateAddPriceCharReserveBytes.disabled = true;
			this.$.updatePriceSize.disabled = false;
		}
	}

	checkPatternAlt() {
		if(this.$.updateUnitDrop.selected == 0) {
			this.$.updateAltSize.allowedPattern = "[0-9kmg]";
			this.$.updateAltSize.pattern = "^[0-9]+[kmg]?$";
		} else if(this.$.updateUnitDrop.selected == 1) {
			this.$.updateAltSize.allowedPattern = "[0-9]";
			this.$.updateAltSize.pattern = "^[0-9]+$";
		} else if(this.$.updateUnitDrop.selected == 2) {
			this.$.updateAltSize.allowedPattern = "[0-9mh]";
			this.$.updateAltSize.pattern = "^[0-9]+[mh]?$";
		}
	}

	checkRecure() {
		if(this.$.updatePriceType.selected == 0) {
			this.$.updatePricePerioddrop.disabled = false;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.updateAddPriceCharReserveTime.disabled = true;
			this.$.updateAddPriceCharReserveBytes.disabled = true;
			this.$.updatePriceUnits.selected = 1;
			this.$.updatePriceAmount.disabled = false;
		} else if(this.$.updatePriceType.selected == 1) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.updateAddPriceCharReserveTime.disabled = true;
			this.$.updateAddPriceCharReserveBytes.disabled = true;
			this.$.updatePriceUnits.selected = 1;
			this.$.updatePriceAmount.disabled = false;
		} else if(this.$.updatePriceType.selected == 2) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceBytes.disabled = false;
			this.$.priceSeconds.disabled = false;
			this.$.updatePriceUnits.selected = 0;
			this.$.updatePriceAmount.disabled = false;
		} else if(this.$.updatePriceType.selected == 3) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = false;
			this.$.updatePriceUnits.selected = 2;
			this.$.updatePriceAmount.disabled = true;
			this.$.updatePriceAmount.value = null;
		}
	}

	checkRecureAlt() {
		if(this.$.updateAltType.selected == 0) {
			this.$.addAltPeriodDrop.disabled = false;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.updateUnitDrop.selected = 1;
		} else if(this.$.updateAltType.selected == 1) {
			this.$.addAltPeriodDrop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.updateUnitDrop.selected = 1;
		} else if(this.$.updateAltType.selected == 2) {
			this.$.addAltPeriodDrop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = true;
			this.$.updateUnitDrop.selected = 0;
		}
	}

	updateAddPrice(event) {
		function updateCheckPriceName(updatePrice) {
			return updatePrice.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		var updateIndexPrice = this.prices.findIndex(updateCheckPriceName);
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
		var charAddObj = new Object();
		charAddObj.priceCharReserveTime = this.$.updateAddPriceCharReserveTime.value;
		charAddObj.priceCharReserveBytes = this.$.updateAddPriceCharReserveBytes.value;
		charAddObj.timeOfDayStart = this.startTimeUpdate;
		charAddObj.timeOfDayEnd = this.endTimeUpdate;
		updatePriceNew.prodSpecCharValueUse = charAddObj;
		if(this.priceUpdateAlteration) {
			function checkAlt(alts) {
				return alts.name == this.priceUpdateAlteration;
			}
			updatePriceNew.alteration = this.alterations.findIndex(checkAlt);
		}
		if(updatePriceNew.name
					&& (updatePriceNew.amount || updatePriceNew.updatePriceType == "tariff") 
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
			if(this.updateOfferStartDatePrice) {
				var startTimeObj = new Object();
				startTimeObj.startDateTime = this.updateOfferStartDatePrice; 
				addValue.validFor = startTimeObj;
			}
			if(this.updateOfferEndDatePrice) {
				var endTimeObj = new Object();
				endTimeObj.endDateTime = this.updateOfferEndDatePrice;
				addValue.validFor = endTimeObj;
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
			if(this.priceUpdateUnits == "Seconds") {
				this.priceUpdateUnits = "s";
			}
			if(this.priceUpdateUnits == "Bytes") {
				this.priceUpdateUnits = "b";
			}
			if(this.priceUpdateUnits && this.priceUpdateSize) {
				var len = this.priceUpdateSize.length;
 				var m = this.priceUpdateSize.charAt(len - 1);
 				if(isNaN(parseInt(m))) {
 					var s = this.priceUpdateSize.slice(0, (len - 1));
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
			if(this.$.updateAddPriceCharReserveTime.value) {
				addValue.radiusReserveTime = this.$.updateAddPriceCharReserveTime.value;
			}
			if(this.$.updateAddPriceCharReserveBytes.value) {
				addValue.radiusReserveOctets = this.$.updateAddPriceCharReserveBytes.value;
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
				prodSpec.id = "3";
				prodSpec.href = "/catalogManagement/v2/productSpecification/3";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.priceAddRoaming) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "roamingTable";
				charValueUse.minCardinality = 0;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				charValue.value = this.priceAddRoaming;
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
			if (this.chargingKey) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "chargingKey";
				charValue.value = parseInt(this.chargingKey);
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
			if (this.$.updateAddPriceCharReserveTime.value) {
				var charValue = new Object();
				charValue.unitOfMeasure = "seconds";
				var charLength = this.$.updateAddPriceCharReserveTime.value.length;
				var lastChar = this.$.updateAddPriceCharReserveTime.value.charAt(charLength - 1);
				if(isNaN(parseInt(lastChar))) {
					var s = this.$.updateAddPriceCharReserveTime.value.slice(0, (charLength -1));
				} else {
					var s = this.$.updateAddPriceCharReserveTime.value;
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
				charValueUse.valueType = "Number";
				charValueUse.minCardinality = 1;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "4";
				prodSpec.href = "/catalogManagement/v2/productSpecification/4";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.$.updateAddPriceCharReserveBytes.value) {
				var charValue = new Object();
				charValue.unitOfMeasure = "octets";
				var charLength = this.$.updateAddPriceCharReserveBytes.value.length;
				var lastChar = this.$.updateAddPriceCharReserveBytes.value.charAt(charLength - 1);
				if(isNaN(parseInt(lastChar))) {
					var s = this.$.updateAddPriceCharReserveBytes.value.slice(0, (charLength -1));
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
				charValueUse.valueType = "Number";
				charValueUse.minCardinality = 1;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				var charValuesByte = new Array();
				charValuesByte.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValuesByte;
				var prodSpec = new Object();
				prodSpec.id = "4";
				prodSpec.href = "/catalogManagement/v2/productSpecification/4";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
				addValue.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			if (this.startTimeUpdate || this.endTimeUpdate) {
				if (this.startTimeUpdate.length > 0 || this.endTimeUpdate.length > 0) {
					var charValueUse = new Object();
					charValueUse.name = "timeOfDayRange";
					charValueUse.valueType = "Range";
					charValueUse.minCardinality = 0;
					charValueUse.maxCardinality = 1;
					var charValue1 = new Object();
					var charValue = new Object();
					var charValueLower = new Object();
					charValueLower.amount = this.startTimeUpdate;
					charValueLower.units = "minutes";
					charValue.lowerValue = charValueLower;
					var charValueUpper = new Object();
					charValueUpper.amount = this.endTimeUpdate;
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
			this.priceUpdateName = null;
			this.priceUpdateDescription = null;
			this.updateOfferStartDatePrice = null;
			this.priceUpdateSize = null;
			this.$.updatePriceAmount.value = null;
			this.priceUpdateUnits = null;
			this.updateOfferEndDatePrice = null;
			this.priceUpdateType = null;
			this.priceUpdateCurrency = null;
			this.priceUpdatePeriod = null;
			this.$.updateAddPriceCharReserveTime.value = null;
			this.$.updateAddPriceCharReserveBytes.value = null;
			this.priceUpdateTariff = null;
			this.$.updateReserveSession.value = null;
			this.updateRedirect = null;
			this.startTimeUpdate = null;
			this.endTimeUpdate = null;
			this.$.updateCheckIn.checked = false;
			this.$.updateCheckOut.checked = false;
			this.priceUpdateAlteration = null;
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	updateAddAlteration() {
		function updateCheckAltName(updateAlts) {
			return updateAlts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;
		}
		var updateIndexAlt = this.alterations.findIndex(updateCheckAltName);
		if(updateIndexAlt == -1) {
			var updateAltNew = new Object();
		} else {
			var updateAltNew = this.alterations[updateIndexAlt];
		}
		updateAltNew.name = this.$.updateAltName.value;
		updateAltNew.description = this.AltUpdateDescription;
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
		switch(this.$.updateUnitDrop.selected) {
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
			this.AltUpdateName = null
			this.AltUpdateDescription = null;
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
		this.updateOfferName = null;
		this.updateOfferDescription = null;
		this.updateOfferSpecification = null;
		this.updateOfferStartDate = null;
		this.updateOfferEndDate = null;
		this.offerUpdateStatus = null;
		this.$.updateAddPriceChars.hide();
		this.$.addBundleUpdate.hide();
		this.priceUpdateName = null;
		this.priceUpdateDescription = null;
		this.updateOfferStartDatePrice = null;
		this.updateOfferEndDatePrice = null;
		this.priceUpdateType = null;
		this.priceUpdateUnits = null;
		this.priceUpdateAmount = null;
		this.priceUpdateSize = null;
		this.priceUpdateCurrency = null;
		this.priceUpdatePeriod = null;
		this.priceUpdateAlteration = null;
		this.AltUpdateName = null;
		this.AltUpdateDescription = null;
		this.updateOfferStartDateAlt = null;
		this.updateOfferEndDateAlt = null;
		this.altUpdateType = null;
		this.$.updateAltSize.value = null;
		this.altUpdateCurrency = null;
		this.$.updateAltAmount.value = null;
		this.AltUpdatePeriod = null;
		this.altUpdateUnits = null;
		this.$.updateAddPriceCharReserveTime.value = null;
		this.$.updateAddPriceCharReserveBytes.value = null;
		this.priceUpdateTariff = null;
		this.priceAddRoaming = null;
		this.chargingKey = null;
		this.$.updateReserveSession.value = null;
		this.updateRedirect = null;
		this.startTimeUpdate = null;
		this.endTimeUpdate = null;
		this.$.updateCheckIn.checked = null;
		this.$.updateCheckOut.checked = null;
		this.$.updateOfferModal.close();
	}
}

window.customElements.define('sig-offer-update', offerUpdate);
