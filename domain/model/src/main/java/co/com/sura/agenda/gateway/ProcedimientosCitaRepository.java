package co.com.sura.agenda.gateway;

import co.com.sura.remision.entity.procedimientos.Canalizacion;
import co.com.sura.remision.entity.procedimientos.Curacion;
import co.com.sura.remision.entity.procedimientos.Fototerapia;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import co.com.sura.remision.entity.procedimientos.Secrecion;
import co.com.sura.remision.entity.procedimientos.Sondaje;
import co.com.sura.remision.entity.procedimientos.SoporteNutricional;
import co.com.sura.remision.entity.procedimientos.TomaMuestra;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ProcedimientosCitaRepository {

    //procedimientos
    Mono<Procedimientos> consultarProcedimientosByIdCita(String idCita);
    //curaciones
    Flux<Curacion> consultarCuracionesByCitas(String idCita);

    //Canalizaciones
    Flux<Canalizacion>      consultarCanalizacionesByCitas(String idCita);

    //Fototerapias
    Flux<Fototerapia>       consultarFototerapiasByCitas(String idCita);

    //Secreciones
    Flux<Secrecion>         consultarSecrecionesByCitas(String idCita);

    //sondaje
    Flux<Sondaje>           consultarSondajesByCitas(String idCita);

    //toma de muestras
    Flux<TomaMuestra>        consultarTomaMuestrasByCitas(String idCita);

    //soporte nutricionales
    Flux<SoporteNutricional> consultarSoporteNutricionalesByCitas(String idCita);
}
