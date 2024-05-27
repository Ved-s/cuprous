#[macro_export]
macro_rules! define_tab_type {
    (
        $(#[$enummeta:meta])*
        $enumvis:vis enum $enumname:ident {
            $(
                #[id = $id:literal]
                #[impl = $impl:ty, loadable = $loadable:ident]
                #[title = $title:literal]
                $membername:ident
            ),*

            $(,)?
        }
    ) => {

        $(#[$enummeta])*
        $enumvis enum $enumname {
            $($membername),*
        }

        impl $enumname {
            pub fn from_id(id: &str) -> Option<Self> {
                match id {
                    $($id => Some(Self::$membername),)*
                    _ => None,
                }
            }

            pub fn id(&self) -> &'static str {
                match self {
                    $(Self::$membername => $id),*
                }
            }

            pub fn title(&self) -> &'static str {
                match self {
                    $(Self::$membername => $title),*
                }
            }

            pub fn create_impl(&self) -> Box<dyn TabImpl> {
                match self {
                    $(Self::$membername => Box::new(<$impl as TabCreation>::new())),*
                }
            }

            pub fn load_impl(&self, data: &str) -> Result<Box<dyn TabImpl>, Box<dyn std::error::Error>> {
                Ok(match self {
                    $(Self::$membername => Box::new(<$impl as TabCreation>::load(data)?)),*
                })
            }
        }
    };
}