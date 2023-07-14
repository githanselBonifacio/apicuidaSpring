package co.com.sura.web.agenda;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.entity.agenda.Profesional;
import co.com.sura.entity.remision.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Date;


@RestController
@RequestMapping("/agenda")
public class AgendaController {

    @Autowired
    AgendaUseCase agendaUseCase;

    //profesionales
    @GetMapping(value = "/profesionales")
    public Flux<Profesional> getProfesionales(){
        return agendaUseCase.consultarProfesionales();
    }

    @GetMapping(value = "/profesionalesbyCiudad/{idCiudad}")
    public Flux<Profesional> getProfesionalesByCiudad(@PathVariable String idCiudad){
        return agendaUseCase.consultarProfesionalesByCiudad(idCiudad);
    }

    @PostMapping(value = "/crearProfesional")
    public Mono<Profesional> crearProfesional(@RequestBody Profesional profesional) {
        return agendaUseCase.crearProfesional(profesional);

    }

    @PutMapping(value = "/actualizarProfesional")
    public Mono<Profesional> actualizarProfesional(@RequestBody Profesional profesional) {
        return agendaUseCase.actualizarProfesional(profesional);

    }

    @GetMapping(value = "/citas")
    public Flux<Cita> getCitasByTurnoCiudad(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idCiudad) {
        return agendaUseCase.consultarCitasByTurnoCiudad(fechaTurno,idHorarioTurno, idCiudad);

    }

    @GetMapping(value = "/asignarProfesionalCita")
    public Mono<Void> asignarProfesionalCita(@RequestParam String idCita,@RequestParam String idProfesional) {
        return agendaUseCase.asignarProfesionaCita(idCita,idProfesional);

    }
    @GetMapping(value = "/desasignarProfesionalCita")
    public Mono<Void> desasignarProfesionalCita(@RequestParam String idCita) {
        return agendaUseCase.desasignarProfesionaCita(idCita);

    }
    @GetMapping(value = "/tratamientos")
    public Flux<Tratamiento> consultarTratamientosByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarTratamientosByCita(idCita);

    }
    @GetMapping(value = "/procedimiento/curaciones")
    public Flux<Curacion> consultarCuracionesByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarCuracionesByCita(idCita);

    }
    @GetMapping(value = "/procedimiento/canalizaciones")
    public Flux<Canalizacion> consultarCanalizacionesByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarCanalizacionesByCita(idCita);

    }

    @GetMapping(value = "/procedimiento/fototerapias")
    public Flux<Fototerapia> consultarFototerapiasByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarFototerapiasByCita(idCita);

    }

    @GetMapping(value = "/procedimiento/secreciones")
    public Flux<Secrecion> consultarSecrecionesByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarSecrecionesByCita(idCita);

    }
    @GetMapping(value = "/procedimiento/sondajes")
    public Flux<Sondaje> consultarSondajesByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarSondajeByCita(idCita);

    }

    @GetMapping(value = "/procedimiento/tomaMuestras")
    public Flux<TomaMuestra> consultarTomaMuestrasByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarTomaMuestrasByCita(idCita);

    }

    @GetMapping(value = "/procedimiento/soporteNutricional")
    public Flux<SoporteNutricional> consultarSoporteNutricionalesByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarSoporteNutricionalesByCita(idCita);

    }
}
