// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="Overview.html">Overview</a></li><li class="chapter-item expanded "><a href="Download.html"><strong aria-hidden="true">1.</strong> Download</a></li><li class="chapter-item expanded "><a href="Kernel.html"><strong aria-hidden="true">2.</strong> Kernel</a></li><li class="chapter-item expanded "><a href="Handling.html"><strong aria-hidden="true">3.</strong> Handling</a></li><li class="chapter-item expanded "><a href="Promises.html"><strong aria-hidden="true">4.</strong> Promises</a></li><li class="chapter-item expanded "><a href="Cognates.html"><strong aria-hidden="true">5.</strong> Cognates</a></li><li class="chapter-item expanded "><a href="pmap.html"><strong aria-hidden="true">6.</strong> pmap</a></li><li class="chapter-item expanded "><a href="preduce.html"><strong aria-hidden="true">7.</strong> preduce</a></li><li class="chapter-item expanded "><a href="defpun.html"><strong aria-hidden="true">8.</strong> defpun</a></li><li class="chapter-item expanded "><a href="Ptrees.html"><strong aria-hidden="true">9.</strong> Ptrees</a></li><li class="chapter-item expanded "><a href="Benchmarks.html"><strong aria-hidden="true">10.</strong> Benchmarks</a></li><li class="chapter-item expanded "><a href="API.html"><strong aria-hidden="true">11.</strong> API</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="API-Kernel.html"><strong aria-hidden="true">11.1.</strong> Kernel</a></li><li class="chapter-item expanded "><a href="API-Promises.html"><strong aria-hidden="true">11.2.</strong> Promises</a></li><li class="chapter-item expanded "><a href="API-Cognates.html"><strong aria-hidden="true">11.3.</strong> Cognates</a></li><li class="chapter-item expanded "><a href="API-Ptrees.html"><strong aria-hidden="true">11.4.</strong> Ptrees</a></li><li class="chapter-item expanded "><a href="API-defpun.html"><strong aria-hidden="true">11.5.</strong> defpun</a></li><li class="chapter-item expanded "><a href="API-Queues.html"><strong aria-hidden="true">11.6.</strong> Queues</a></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
