/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
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
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import './style-element.js';

class productAddClass extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addProductInvenModal" modal>
				<app-toolbar>
					<h2>Add Product</h2>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-dropdown-menu
						id="addProDrop"
						label="Product Offer"
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
				<paper-input
						id="servId"
						name="ServiceId"
						value="{{proService}}"
						label="Service Id">
				</paper-input>
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
					<h2>Delete Product</h2>
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
							on-tap="cancelDel">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="deleteProductAjax"
					url="/productInventoryManagement/v2/product/"
					method="delete"
					on-response="_deleteProductResponse"
					on-error="_deleteProductrror">
			</iron-ajax>
			<iron-ajax
					id="addProductAjax"
					url="/productInventoryManagement/v2/product"
					method = "post"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addProductResponse"
					on-error="_addProductrror">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			selected: {
				type: Number,
				value: 0
			},
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
		this.$.deleteProductModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('productList').shadowRoot.getElementById('getProductInventory').generateRequest();
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
//		var productServi = new Array();
//		var services = this.proService;
//		for(var index in services) {
//			bla = new Object();
//			bla.id = services[index];
//			bla.href = "/serviceInventoryManagement/v2/service/" + services[index];
//			productServi.push(bla);
//		}
		productRes.realizingService= productServi;
		ajaxPro.body = productRes;
		ajaxPro.generateRequest();
		this.proAdd = null;
		this.proService = null;
		this.$.addProductInvenModal.close();
	}

	_addProductResponse() {
		this.$.addProductInvenModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('productList').shadowRoot.getElementById('productInventoryGrid').clearCache();
	}
}

window.customElements.define('sig-product-add', productAddClass);
