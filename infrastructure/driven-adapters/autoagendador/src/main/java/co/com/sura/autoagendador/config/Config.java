package co.com.sura.autoagendador.config;

import co.com.sura.autoagendador.models.AutoAgendador;
import co.com.sura.services.mapbox.MapboxServiceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Config {
    public static final Integer MAXSIZE                       = 2;
    public static final Integer NUMERO_GENERACIONES           = 1000;
    public static final Integer SIZE_POBLACION_INICIAL        = 10;
    public static final Integer NUMERO_PADRES_EMPAREJADOS     = 5;
    public static final Integer HOLGURA_DEFECTO               = 1200;
    public static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;


    public final MapboxServiceRepository mapboxService;

    @Autowired
    public Config(MapboxServiceRepository mapboxService) {
        this.mapboxService = mapboxService;
    }

    @Bean
    public AutoAgendador autoAgendador(){
        return new AutoAgendador(NUMERO_GENERACIONES,
                SIZE_POBLACION_INICIAL,NUMERO_PADRES_EMPAREJADOS,
                PENALIZACION_HOLGURA_NEGATIVA,mapboxService);
  }
}
