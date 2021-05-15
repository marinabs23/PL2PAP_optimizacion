import React, { Component } from "react";

export default class SignUp extends Component {
    render() {
        return (
            <form>
                <h3>Registrarse</h3>

                <div className="form-group">
                    <label>Nombre</label>
                    <input type="text" className="form-control" placeholder="Nombre" />
                </div>

                <div className="form-group">
                    <label>Apellido</label>
                    <input type="text" className="form-control" placeholder="Apellido" />
                </div>

                <div className="form-group">
                    <label>Email</label>
                    <input type="email" className="form-control" placeholder="Correo electrónico" />
                </div>

                <div className="form-group">
                    <label>Contraseña</label>
                    <input type="password" className="form-control" placeholder="Contraseña" />
                </div>

                <button type="submit" className="btn btn-dark btn-lg btn-block">Registrarse</button>
                <p className="forgot-password text-right">
                    ¿Ya tienes una cuenta? <a href="#">Iniciar Sesión</a>
                </p>
            </form>
        );
    }
}