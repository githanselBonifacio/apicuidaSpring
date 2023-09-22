package co.com.sura.web.remision;


import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.dto.remision.CrearRemisionCitasRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.remision.DatosAtencionPaciente;
import co.com.sura.entity.remision.Paciente;
import co.com.sura.entity.remision.RegistroHistorialRemision;
import co.com.sura.entity.remision.Remision;
import co.com.sura.genericos.Response;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;


@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/remision")
public class RemisionController {

    @Autowired
    private RemisionUseCase remisionUseCase;

    //remision
    @PostMapping(value = "/crearRemisionCitas")
    public Mono<Response<Boolean>> crearRemisioCitas(@RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest) {
        return remisionUseCase.crearRemisionCitas(
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
        return remisionUseCase.consultarRemisiones()
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
        return remisionUseCase.consultarDatosAtencionPacienteByRemision(idRemision)
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
        return remisionUseCase.consultarPacienteFromRemision(idRemision)
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

    @GetMapping(value = "tratamientosFarmacia")
    public Mono<Response<List<PacienteTratamientoCita>>> consultarMedicamentosToFarmacia(){
        return remisionUseCase.consultarAllTratamientosToFarmacia()
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
        return remisionUseCase.consultarAllTratamientosToFarmaciaWithFilter(fechaTurno,idHorarioTurno,idRegional)
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
        return remisionUseCase.notificarMedicamentosToFarmacia(tratamientoCitasList)
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
        return remisionUseCase.consultarHistorialRemisionById(idRemision)
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
        return remisionUseCase.consultarDataActualRemision(idRemision)
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
        return remisionUseCase.actualizarRemisionPorNovedad(
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
        return remisionUseCase.egresarRemisionById(idRemision)
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
