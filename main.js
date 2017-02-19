(function () {
	init();

	function init() {
		addEventListeners();	
	}

	function addEventListeners() {
		document.querySelector("#toggle").addEventListener("click", toggleNav);
		var emailButton = document.querySelector(".nav-item a[href^='mailto:']");
		emailButton.addEventListener("mouseenter", toggleEnvelope);
		emailButton.addEventListener("mouseleave", toggleEnvelope);
	}

	function toggleNav() {
		var nav = document.querySelector(".nav-menu");
		nav.style.display == "" ?
			nav.style.display = "block" :
			nav.style.display = "" ;
	}

	function toggleEnvelope(e) {
		var i = e.currentTarget.querySelector("i");
		if (i.classList.contains("fa-envelope")) {
			i.classList.remove("fa-envelope");
			i.classList.add("fa-envelope-open");
		} else {
			i.classList.add("fa-envelope");
			i.classList.remove("fa-envelope-open");
		}
	}

})();