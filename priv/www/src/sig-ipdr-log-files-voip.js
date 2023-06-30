/**
 * Copyright 2016 - 2023 SigScale Global Inc.
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
import '@polymer/paper-dialog-scrollable/paper-dialog-scrollable.js';
import '@polymer/iron-list/iron-list.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class ipdrLogVoip extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="selectLogFileModalVoip" modal>
				<app-toolbar>
					<h2>Select Log File</h2>
				</app-toolbar>
				<paper-dialog-scrollable>
					<iron-list id="logFilesVoip"
							as="item">
						<template>
							<div class="item" on-tap="getLogContentVoip">
								<iron-icon icon="assignment"></iron-icon>
										[[item]]
							</div>
						</template>
					</iron-list>
				</paper-dialog-scrollable>
				<div class="cancel-button">
					<paper-button dialog-dismiss>
							Cancel
					</paper-button>
				</div>
			</paper-dialog>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			}
		}
	}

	ready() {
		super.ready();
	}

	getLogContentVoip(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-list-voip').shadowRoot.getElementById("ipdrListVoip").intializeGrid(event);
	}
}

window.customElements.define('sig-ipdr-log-files-voip', ipdrLogVoip);
