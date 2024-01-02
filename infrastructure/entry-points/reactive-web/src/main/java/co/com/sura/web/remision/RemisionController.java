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
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;
import java.util.List;


@RestController
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping("/remision")
public class RemisionController {

    @Autowired
    private RemisionUseCase adminUseCase;

    //remision
    /**
     * crear remisión
     * @param crearRemisionCitasRequest request recibida para crear todos los registros en todas las tablas en base de datos
     *                                  (co.com.sura.remision.dto.CrearRemisionCitasRequest.class)
     * @return remisión creada
     * */
    @PostMapping(value = "/crearRemisionCitas")
    public Mono<Response<Boolean>> crearRemisioCitas(@RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest) {
        return adminUseCase.crearRemisionCitas(
                        crearRemisionCitasRequest.getRemision(),
                        crearRemisionCitasRequest.getCitas())
                .map(fueCreada -> ResponseFactory.createStatus(
                        fueCreada,
                        StatusCode.STATUS_200,
                        Mensajes.REMISION_CREADA,
                        Mensajes.REMISION_CREADA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        e.getMessage().contains(
                                Mensajes.REMISION_EXISTENTE.split("d")[0])?
                                StatusCode.STATUS_400:StatusCode.STATUS_500,
                        Mensajes.ERROR_CREAR_REMISION,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()

                )));
    }
    /**
     * actualizar remisión
     * @param crearRemisionCitasRequest request recibida para actualizar todos los registros en todas las tablas en base de datos
     *                                  (co.com.sura.remision.dto.CrearRemisionCitasRequest.class)
     * @return remisión actualizada
     * */
    @PostMapping("/actualizarRemisionPorNovedad")
    public Mono<Response<Boolean>> actualizarRemisionPorNovedad(
            @RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest){
        return adminUseCase.actualizarRemisionPorNovedad(
                        crearRemisionCitasRequest.getRemision(),
                        crearRemisionCitasRequest.getCitas(),
                        crearRemisionCitasRequest.getNovedad())
               .map(fueActualizada ->ResponseFactory.createStatus(
                        fueActualizada,
                        StatusCode.STATUS_200,
                        Mensajes.REMISION_ACTUALIZADA,
                        Mensajes.REMISION_ACTUALIZADA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_ACTUALIZAR_REMISION,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    /**
     * consultar todas las remisiones
     * @return remisiones (Response<List<co.com.sura.remision.entity.Remision>)
     * */
    @GetMapping(value = "")
    public Mono<Response<List<Remision>>> consultarRemisiones(){
        return adminUseCase.consultarRemisiones()
                .collectList()
                .map(remisiones -> ResponseFactory.createStatus(
                        remisiones,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));

    }

    /**
     * consultar datos de atención de remisión
     * @param idRemision id remisión consultada
     * @return remisiones (Response<co.com.sura.remision.entity.DatosAtencionPaciente>)
     * */
    @GetMapping(value = "datosAtencionPaciente/{idRemision}")
    public Mono<Response<DatosAtencionPaciente>> consultarDatosAtencionPacienteByRemision(
            @PathVariable String idRemision){
        return adminUseCase.consultarDatosAtencionPacienteByRemision(idRemision)
                .map(datosAtencion -> ResponseFactory.createStatus(
                        datosAtencion,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    /**
     * consultar paciente by remisión
     * @param idRemision id remisión consultada
     * @return remisiones (Response<co.com.sura.remision.entity.Paciente>)
     * */
    @GetMapping(value = "pacienteRemision/{idRemision}")
    public Mono<Response<Paciente>> consultarPacienteFromRemision(@PathVariable String idRemision){
        return adminUseCase.consultarPacienteFromRemision(idRemision)
                .map(paciente -> ResponseFactory.createStatus(
                        paciente,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    //remisiones
    /**
     * consultar historial de cambios de remisión
     * @param idRemision id remisión consultada
     * @return remisiones (Response<List<co.com.sura.remision.entity.RegistroHistorialRemision>>)
     * */
    @GetMapping(value = "historial/{idRemision}")
    public Mono<Response<List<RegistroHistorialRemision>>> consultarHistorialRemisionById(
            @PathVariable String idRemision){
        return adminUseCase.consultarHistorialRemisionById(idRemision)
                .collectList()
                .map(historial -> ResponseFactory.createStatus(
                        historial,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    /**
     * consultar datos actuales de remisión
     * @param idRemision id remisión consultada
     * @return remisiones (Response<List<co.com.sura.remision.entity.RegistroHistorialRemision>>)
     * */
    @GetMapping(value = "/{idRemision}")
    public Mono<Response<RegistroHistorialRemision>> consultarAllDataRemisionById(@PathVariable String idRemision){
        return adminUseCase.consultarDataActualRemision(idRemision)
               .map(registroHistorial -> ResponseFactory.createStatus(
                        registroHistorial,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage(),
                        e.getMessage()
                )));
    }


    /**
     * egresar remisión por ID remisión
     * @param idRemision id remision a egresar (String)
     * @return remision egresada (Response<Boolea>)
     * */
    @PutMapping(value="/egresar")
    public Mono<Response<Boolean>> egresarRemisionById(@RequestParam String idRemision) {
        return adminUseCase.egresarRemisionById(idRemision)
                .map(fueEgresada -> ResponseFactory.createStatus(
                        fueEgresada,
                        StatusCode.STATUS_200,
                        Mensajes.REMISION_EGRESADA,
                        Mensajes.REMISION_EGRESADA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_EGRESAR_REMISION,
                        e.getMessage(),
                        e.getMessage()
                )));
    }


}
