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
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class productAddClass extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addProductInvenModal" modal>
				<app-toolbar>
					<h3>Add Product</h3>
				</app-toolbar>
				<paper-dropdown-menu
						id="addProDrop"
						label="Product Offer"
						no-animations="true"
						value="{{proAdd}}">
					<paper-listbox
							id="addProDropList"
							slot="dropdown-content">
						<template is="dom-repeat" items="[[offers]]">
							<paper-item>
								{{item}}
							</paper-item>
						</template>
					</paper-listbox>
				</paper-dropdown-menu>
				<paper-tooltip
						for="addProDrop"
						offset="0">
					Select an product from the product list
				</paper-tooltip>
				<paper-input
						id="servId"
						name="ServiceId"
						value="{{proService}}"
						label="Service Id">
				</paper-input>
				<paper-tooltip
						for="servId"
						offset="0">
					service ID
				</paper-tooltip>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							class="submit-button"
							on-tap="_productinvenAddSubmit">
						Add
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							on-tap="cancelPro">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<paper-dialog
					class="dialog" id="deleteProductModal" modal>
				<app-toolbar>
					<h3>Delete Product</h3>
				</app-toolbar>
				<div>
					<paper-input
							id="deleteProId"
							name="ProductId"
							value="{{delProId}}"
							label="Product Id"
							required
							disabled>
					</paper-input>
				</div>
				<div class="buttons">
					<paper-button raised
							id="proDelButton"
							on-tap="productDelete"
							class="delete-button">
						Delete
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							on-tap="cancelPro">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="deleteProductAjax"
					url="/productInventoryManagement/v2/product/"
					method="delete"
					on-response="_deleteProductResponse"
					on-error="_deleteProductError">
			</iron-ajax>
			<iron-ajax
					id="addProductAjax"
					url="/productInventoryManagement/v2/product"
					method = "post"
					content-type="application/json"
					on-response="_addProductResponse"
					on-error="_addProductError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			offers: {
				type: Array,
				value: function() {
					return []
				}
			},
			product: {
				type: String
			},
			delProId: {
				type: String
			},
			proService: {
				type: String
			},
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item != null) {
			this.delProId = item.id;
			this.$.deleteProductModal.open();
		}
	}

	productDelete() {
		var delProduct = this.delProId;
		var deleteAjax = this.$.deleteProductAjax;
		deleteAjax.url = "/productInventoryManagement/v2/product/" + delProduct;
		deleteAjax.generateRequest();
	}

	_deleteProductResponse() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('productList').shadowRoot.getElementById('getProductInventory').generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('productList').shadowRoot.getElementById('productInventoryGrid').clearCache();
	}

	_deleteProductError() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_productinvenAddSubmit() {
		var ajaxPro = this.$.addProductAjax;
		var productRes = new Object();
		var productAdd = new Object();
		productAdd.id = this.proAdd;
		productAdd.name = this.proAdd;
		productAdd.href = "/catalogManagement/v2/productOffering/" + this.proAdd; 
		productRes.productOffering = productAdd;
		var productServi = new Array();
		var productSer = new Object();
		productSer.id = this.proService;
		productSer.href = "/serviceInventoryManagement/v2/service/" + this.proService;
		productServi.push(productSer);
		if(productServi.length >= 1) {
			productRes.realizingService= productServi;
		}
		ajaxPro.body = productRes;
		ajaxPro.generateRequest();
		this.$.addProDropList.selected = null;
		this.proAdd = null;
		this.proService = null;
		this.$.addProductInvenModal.close();
	}

	_addProductResponse() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('productList').shadowRoot.getElementById('productInventoryGrid').clearCache();
	}

	_addProductError() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	cancelPro() {
		this.proAdd = null;
		this.$.addProDropList.selected = null;
		this.proService = null;
	}
}

window.customElements.define('sig-product-add', productAddClass);
