// vite.config.js
import { defineConfig } from 'vite';
import cherry from 'cherry-cljs/vite.js';

export default defineConfig(() => {
  return {
    plugins: [cherry()],
  };
});
