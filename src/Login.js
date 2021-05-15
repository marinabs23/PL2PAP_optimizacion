import React, { Component } from "react";

export default class Login extends Component {
    render() {
        return ( 
       
            <form>
                <h3>Inicio de Sesión</h3>
                
                <div className="form-group">
                    <label>Email</label>
                    <input type="email" className="form-control" placeholder="Introduzca su nombre de usuario" />
                </div>

                <div className="form-group">
                    <label>Contraseña</label>
                    <input type="password" className="form-control" placeholder="Introduzca su contraseña" />
                </div>

                <br></br>

                <button type="submit" className="btn btn-dark btn-lg btn-block">Iniciar Sesión</button>
                <p className="forgot-password text-right">
                   ¿No tienes una cuenta? <a href="/sign-up">Registrate</a>
                </p>
            </form>
            
        );
       
    }
   
}