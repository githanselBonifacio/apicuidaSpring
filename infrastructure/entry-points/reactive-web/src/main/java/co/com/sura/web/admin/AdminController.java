package co.com.sura.web.admin;


import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.dto.remision.CrearRemisionCitasRequest;
import co.com.sura.entity.agenda.Conductor;
import co.com.sura.entity.agenda.Movil;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.admin.DatosAtencionPaciente;
import co.com.sura.entity.admin.Paciente;
import co.com.sura.entity.admin.RegistroHistorialRemision;
import co.com.sura.entity.admin.Remision;
import co.com.sura.entity.agenda.Profesional;
import co.com.sura.genericos.Response;
import co.com.sura.admin.AdminUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;


@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/admin")
public class AdminController {

    @Autowired
    private AdminUseCase adminUseCase;

    //profesionales
    @GetMapping(value = "/profesionales")
    public Mono<Response<List<Profesional>>> getProfesionales(){
        return adminUseCase.consultarProfesional()
                .collectList()
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @PostMapping(value = "/crearProfesional")
    public Mono<Response<Profesional>> crearProfesional(@RequestBody Profesional profesional) {
        return adminUseCase.crearProfesional(profesional)
                .map(profesionalCreado -> ResponseFactory.createStatus(
                        profesionalCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_CREA_PROFESIONAL.getValue(),
                        Mensajes.SE_CREA_PROFESIONAL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_PROFESIONAL.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarProfesional")
    public Mono<Response<Profesional>> actualizarProfesional(@RequestBody Profesional profesional) {
        return adminUseCase.actualizarProfesional(profesional)
                .map(profesionalCreado -> ResponseFactory.createStatus(
                        profesionalCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_PROFESIONAL.getValue(),
                        Mensajes.SE_ACTUALIZA_PROFESIONAL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTTUALIZAR_PROFESIONAL.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    //conductores
    @GetMapping(value = "/conductores")
    public Mono<Response<List<Conductor>>> getConductores(){
        return adminUseCase.consultarConductores()
                .collectList()
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @PostMapping(value = "/crearConductor")
    public Mono<Response<Conductor>> crearProfesional(@RequestBody Conductor conductor) {
        return adminUseCase.crearConductor(conductor)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_CREA_CONDUCTOR.getValue(),
                        Mensajes.SE_CREA_CONDUCTOR.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_CONDUCTOR.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarConductor")
    public Mono<Response<Conductor>> actualizarProfesional(@RequestBody Conductor conductor) {
        return adminUseCase.actualizarConductor(conductor)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_CONDUCTOR.getValue(),
                        Mensajes.SE_ACTUALIZA_CONDUCTOR.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_CONDUCTOR.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    //moviles
    @PostMapping(value = "/crearMovil")
    public Mono<Response<Movil>> crearProfesional(@RequestBody Movil movil) {
        return adminUseCase.crearMovil(movil)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_CREA_MOVIL.getValue(),
                        Mensajes.SE_CREA_MOVIL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_MOVIL.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarMovil")
    public Mono<Response<Movil>> actualizarMovil(@RequestBody Movil movil) {
        return adminUseCase.actualizarMovil(movil)
                .map(movilCreado -> ResponseFactory.createStatus(
                        movilCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_MOVIL.getValue(),
                        Mensajes.SE_ACTUALIZA_MOVIL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_MOVIL.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    @GetMapping(value = "/moviles")
    public Mono<Response<List<Movil>>> consultarMoviles(){
        return adminUseCase.consultarMoviles()
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "movilesSinConductor")
    public Mono<Response<List<Movil>>> consultarMovilesSinConductor(){
        return adminUseCase.consultarMovilesSinConductor()
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "/movilesByRegional/{idRegional}")
    public Mono<Response<List<Movil>>> consultarMovilesByIdRegional(@PathVariable String idRegional){
        return adminUseCase.consultarMovilesByIdRegional(idRegional)
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    //remision
    @PostMapping(value = "/crearRemisionCitas")
    public Mono<Response<Boolean>> crearRemisioCitas(@RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest) {
        return adminUseCase.crearRemisionCitas(
                        crearRemisionCitasRequest.getRemision(),
                        crearRemisionCitasRequest.getCitas())
                .map(fueCreada -> ResponseFactory.createStatus(
                        fueCreada,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.REMISION_CREADA.getValue(),
                        Mensajes.REMISION_CREADA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        e.getMessage().contains(
                                Mensajes.REMISION_EXISTENTE.getValue().split("d")[0])?
                                StatusCode.STATUS_400.getValue():StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_REMISION.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()

                )));
    }

    @GetMapping(value = "")
    public Mono<Response<List<Remision>>> consultarRemisiones(){
        return adminUseCase.consultarRemisiones()
                .collectList()
                .map(remisiones -> ResponseFactory.createStatus(
                        remisiones,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));

    }

    @GetMapping(value = "datosAtencionPaciente/{idRemision}")
    public Mono<Response<DatosAtencionPaciente>> consultarDatosAtencionPacienteByRemision(
            @PathVariable String idRemision){
        return adminUseCase.consultarDatosAtencionPacienteByRemision(idRemision)
                .map(datosAtencion -> ResponseFactory.createStatus(
                        datosAtencion,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @GetMapping(value = "pacienteFromRemision/{idRemision}")
    public Mono<Response<Paciente>> consultarPacienteFromRemision(@PathVariable String idRemision){
        return adminUseCase.consultarPacienteFromRemision(idRemision)
                .map(paciente -> ResponseFactory.createStatus(
                        paciente,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    //farmacia
    @GetMapping(value = "tratamientosFarmacia")
    public Mono<Response<List<PacienteTratamientoCita>>> consultarMedicamentosToFarmacia(){
        return adminUseCase.consultarAllTratamientosToFarmacia()
                .collectList()
                .map(pacientes -> ResponseFactory.createStatus(
                        pacientes,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "tratamientosFarmaciaWithFilter")
    public Mono<Response<List<PacienteTratamientoCita>>> consultarMedicamentosToFarmaciaWithFilter(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam String idRegional,
            @RequestParam Integer idHorarioTurno
    ){
        return adminUseCase.consultarAllTratamientosToFarmaciaWithFilter(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(pacientes -> ResponseFactory.createStatus(
                        pacientes,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @PostMapping(value = "notificarFarmacia")
    public Mono<Response<Boolean>>notificarMedicamentosToFarmacia(
            @RequestBody List<PacienteTratamientoCita> tratamientoCitasList){
        return adminUseCase.notificarMedicamentosToFarmacia(tratamientoCitasList)
                .map(seNotifico -> ResponseFactory.createStatus(
                        seNotifico,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_NOTIFICO_FARMACIA.getValue(),
                        Mensajes.SE_NOTIFICO_FARMACIA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_NOTIFICO_FARMACIA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @GetMapping(value = "historial/{idRemision}")
    public Mono<Response<List<RegistroHistorialRemision>>> consultarHistorialRemisionById(
            @PathVariable String idRemision){
        return adminUseCase.consultarHistorialRemisionById(idRemision)
                .collectList()
                .map(historial -> ResponseFactory.createStatus(
                        historial,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @GetMapping(value = "/{idRemision}")
    public Mono<Response<RegistroHistorialRemision>> consultarAllDataRemisionById(@PathVariable String idRemision){
        return adminUseCase.consultarDataActualRemision(idRemision)
                .map(registroHistorial -> ResponseFactory.createStatus(
                        registroHistorial,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @PostMapping("/actualizarRemisionPorNovedad")
    public Mono<Response<Boolean>> actualizarRemisionPorNovedad(
            @RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest){
        return adminUseCase.actualizarRemisionPorNovedad(
                crearRemisionCitasRequest.getRemision(),
                crearRemisionCitasRequest.getCitas(),
                crearRemisionCitasRequest.getNovedad())
                .map(fueActualizada ->ResponseFactory.createStatus(
                        fueActualizada,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.REMISION_ACTUALIZADA.getValue(),
                        Mensajes.REMISION_ACTUALIZADA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_REMISION.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @PostMapping(value="/egresar/{idRemision}")
    public Mono<Response<Boolean>> egresarRemisionById(@PathVariable String idRemision) {
        return adminUseCase.egresarRemisionById(idRemision)
                .map(fueEgresada -> ResponseFactory.createStatus(
                        fueEgresada,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.TURNO_DESAGENDADO.getValue(),
                        Mensajes.REMISION_EGRESADA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_EGRESAR_REMISION.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
}
