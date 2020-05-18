const styleElement = document.createElement('dom-module');
styleElement.setAttribute('theme-for', 'vaadin-grid');

styleElement.innerHTML = `<template>
      <style>
         :host {
            @apply(--paper-font-common-base);
            --app-primary-color: #f57f17;
            --app-secondary-color: #aeea00;
            display: block;
         }
         app-header {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            text-align: center;
            background-color: var(--app-primary-color);
            border-bottom: 1px solid #eee;
            color: #fff;
         }
         .toolbar-top {
            background-color: var(--app-primary-color);
         }
         app-header paper-icon-button {
            --paper-icon-button-ink-color: white;
         }
         app-drawer {
            --app-drawer-content-container: {
               padding-top: 10px;
            };
            height: 100%;
            top: 64px;
         }
         .drawer-list {
            box-sizing: border-box;
            width: 100%;
            height: 100%;
            padding: 10px;
            background: white;
            position: relative;
         }
         .drawer-list a {
            display: block;
            padding: 0 24px;
            text-decoration: none;
            color: black;
            line-height: 40px;
         }
      </style>
   </template>`;

styleElement.register('style-element');
