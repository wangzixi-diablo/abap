function JsTagCloud(config) {

    var cloud = document.getElementById(config.CloudId);
    this._cloud = cloud;

    var w = parseInt(this._getStyle(cloud, 'width'));
    var h = parseInt(this._getStyle(cloud, 'height'));
    this.width = (w - 100) / 2;
    this.height = (h - 50) / 2;

    this._items = cloud.getElementsByTagName('a');
    this._count = this._items.length;
    this._angle = 360 / (this._count);
    this._radian = this._angle * (2 * Math.PI / 360);
    this.step = 0;
}

JsTagCloud.prototype._getStyle = function(elem, name) {
    return window.getComputedStyle ? window.getComputedStyle(elem, null)[name] :
            elem.currentStyle[name];
}

JsTagCloud.prototype._render = function() {
    for (var i = 0; i < this._count; i++) {
        var item = this._items[i];
        var thisRadian = (this._radian * i) + this.step;
        var sinV = Math.sin(thisRadian);
        var cosV = Math.cos(thisRadian);

        item.style.left = (sinV * this.width) + this.width + 'px';
        item.style.top = this.height + (cosV * 50) + 'px';
        item.style.fontSize = cosV * 10 + 20 + 'pt';
        item.style.zIndex = cosV * 1000 + 2000;
        item.style.opacity = (cosV / 2.5) + 0.6;
        item.style.filter = " alpha(opacity=" + ((cosV / 2.5) + 0.6) * 100 + ")";
    }
    this.step += 0.007;
}

JsTagCloud.prototype.start = function() {
    setInterval (function(who) {
        return function() {
            who._render();
        };
    } (this), 20);
}
