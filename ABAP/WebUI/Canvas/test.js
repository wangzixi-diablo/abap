function MyCtx(ctx){
    this.ctx = ctx;
}
(function (map){
    for(var k in map){
        MyCtx.prototype[k] = new Function('this.ctx.'+map[k]+'.apply(this.ctx,arguments); return this;');
    }
}({
    B:'beginPath', M:'moveTo', L:'lineTo', A:'arc', Z:'closePath', f:'fill', dI:'drawImage', cR:'clearRect', clip:'clip', save:'save', restore:'restore'
}));
function init(){
    var ctx = document.getElementById("canvas1").getContext('2d');
    var mtx = new MyCtx(ctx), i=-1;
    mtx.ctx.fillStyle='rgba(0,0,0,0.6)';
    function f(){
        mtx.save().dI(img,0,0).B().A(200,150,250,Math.abs(++i%100)*Math.PI/50,Math.PI*2,(i/100|0)%2).L(200,150).Z().clip().dI(img,-400,0).restore();
        setTimeout(f,60);
    }
    f();
}