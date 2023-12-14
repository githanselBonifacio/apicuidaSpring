package co.com.sura.web.remision;


import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.remision.dto.CrearRemisionCitasRequest;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Paciente;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.remision.entity.Remision;
import co.com.sura.genericos.Response;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;
import java.util.List;


@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/remision")
public class RemisionController {

    @Autowired
    private RemisionUseCase adminUseCase;

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

    @GetMapping(value = "pacienteRemision/{idRemision}")
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
    //remisiones
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
                        e.getMessage(),
                        e.getMessage()
                )));
    }



    @PostMapping(value="/egresar/{idRemision}")
    public Mono<Response<Boolean>> egresarRemisionById(@PathVariable String idRemision) {
        return adminUseCase.egresarRemisionById(idRemision)
                .map(fueEgresada -> ResponseFactory.createStatus(
                        fueEgresada,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.REMISION_EGRESADA.getValue(),
                        Mensajes.REMISION_EGRESADA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_EGRESAR_REMISION.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));
    }


}
