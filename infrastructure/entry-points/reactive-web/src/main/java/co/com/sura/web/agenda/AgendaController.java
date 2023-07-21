package co.com.sura.web.agenda;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.autoagendar.DesplazamientoMap;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.Desplazamiento;
import co.com.sura.entity.agenda.Profesional;
import co.com.sura.entity.remision.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;


@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/agenda")
public class AgendaController {

    @Autowired
    AgendaUseCase agendaUseCase;

    //profesionales
    @GetMapping(value = "/profesionales")
    public Flux<Profesional> getProfesionales(){
        return agendaUseCase.consultarProfesionales();
    }

    @GetMapping(value = "/profesionalesByTurnoCiudad")
    public Flux<Profesional> getProfesionalesbyTurnoCiudad(
             @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
             @RequestParam String idCiudad){
        return agendaUseCase.consultarProfesionalesByTurnoCiudad(fechaTurno, idCiudad);
    }

    @GetMapping(value = "/profesionalesFromTurnoCiudad")
    public Flux<Profesional> getProfesionalesfromTurnoCiudad(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam String idCiudad,
            @RequestParam Integer idHorarioTurno){
        return agendaUseCase.consultarProfesionalesFromTurnoCiudad(fechaTurno, idCiudad, idHorarioTurno);
    }

    @GetMapping(value = "/asignarProfesionalTurno")
    public Mono<Void> asignarProfesionalTurno(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idProfesional){
        return agendaUseCase.asignarProfesionalTurno(fechaTurno, idHorarioTurno,idProfesional);
    }

    @GetMapping(value = "/deasignarProfesionalTurno")
    public Mono<Void> desasignarProfesionalTurno(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idProfesional){
        return agendaUseCase.desasignarProfesionalTurno(fechaTurno, idHorarioTurno,idProfesional);
    }

    @GetMapping(value = "/desagendarTurnoCompleto")
    public Mono<Void> desagendarTurnoCompleto(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno){
        return agendaUseCase.desagendarTurnoCompleto(fechaTurno, idHorarioTurno);
    }

    @GetMapping(value = "/actividadesByprofesionalesCiudadHorario")
    public Flux<Actividad> getActividadesByProfesionalesCiudadHorario(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idCiudad){
        return agendaUseCase.consultarActividadesProfesionalesCiudadHorario(fechaTurno,idHorarioTurno,idCiudad);
    }
    @GetMapping(value = "/desplazamientoVisita")
    public Flux<Desplazamiento> getDesplazamientoByIdCitaPartida(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idCiudad ){
        return agendaUseCase.consultarDesplazamientoByIdCitaPartida(fechaTurno,idHorarioTurno,idCiudad);
    }

    @GetMapping(value = "/calcularDesplazamientoCitasByprofesional")
    public Mono<Void> calcularDesplazamientoCitaByProfesional(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idCiudad,
            @RequestParam String idProfesional){
        return agendaUseCase.calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idCiudad,idProfesional);
    }

    @GetMapping(value = "/profesionales/{idCiudad}")
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
    @GetMapping(value = "/reprogramarCita")
    public Mono<Void> reprogramarCita(
            @RequestParam("fechaProgramada")  String fechaProgramada,
            @RequestParam String idCita,
            @RequestParam String nuevaHora) {

        var hora = nuevaHora.split(":");

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        LocalDateTime localDateTime = LocalDateTime.parse(fechaProgramada, formatter)
                .withHour(Integer.parseInt(hora[0])).withMinute(Integer.parseInt(hora[1]));

        return agendaUseCase.reprogramarCitaById(localDateTime,idCita);

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
