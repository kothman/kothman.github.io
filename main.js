(function () {
	init();

	function init() {
		addEventListeners();	
	}

	function addEventListeners() {
		document.querySelector("#toggle").addEventListener("click", toggleNav);
	}

	function toggleNav() {
		var nav = document.querySelector(".nav-menu");
		nav.style.display == "" ?
			nav.style.display = "block" :
			nav.style.display = "" ;
	}

})();