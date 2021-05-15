
import React, { Component } from "react";
import '../node_modules/bootstrap/dist/css/bootstrap.min.css';
import './App.css';
import logo from "./images/logo_cromos.png";
import background from "./images/fondo_login.png";
import { BrowserRouter as Router, Switch, Route, Link } from "react-router-dom";

import Login from "./Login";
import SignUp from "./Singup";

export default class App extends Component {
  constructor(props) {
    super(props);
    this.state = { fondo: `url(${background})`};
  }

  changeFondo = fondo => {
    this.setState({ fondo });
  };

  render() {
  return (
  <Router>
    <div className="App fondo" style={{ backgroundImage: this.state.fondo}} >
      <nav className="navbar navbar-expand-lg navbar-light fixed-top">
        <div className="container">
        <img src={logo} className="logo"></img>
          <Link className="navbar-brand" to={"/login"}>FlashCards</Link>
          <div className="collapse navbar-collapse" >
            <ul className="navbar-nav ml-auto">
              <li className="nav-item">
                <Link className="nav-link" to={"/login"} onClick={() => this.changeFondo(`url(${background})`)}>Iniciar Sesión</Link>
              </li>
              <li className="nav-item">
                <Link className="nav-link" to={"/registro"} onClick={() => this.changeFondo(`url(${background})`)}>Registrarse</Link>
              </li>
            </ul>
          </div>
        </div>
      </nav>

      <div className="outer">
        <div className="inner">
          <Switch>
            <Route exact path='/' component={Login} />
            <Route path="/login" component={Login} />
            <Route path="/registro" component={SignUp} />
          </Switch>
        </div>
      </div>
    </div></Router>


  );
}

}